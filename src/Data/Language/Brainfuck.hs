{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Data.Language.Brainfuck
    ( boot
    , compile
    , run

    , Program
    , Machine()
    , InterpreterException(..)
    )
where

import qualified Data.Vector.Mutable as MV
import Control.Exception (throwIO, Exception)
import Control.Monad (foldM)
import Data.Char (ord, chr)
import Data.Functor ((<$>))
import Data.Typeable (Typeable)

data Instruction
    = AdjustCell Int
    | AdjustCellPtr Int
    | PutChar
    | GetChar
    | Loop Program
    | SetCell Int
    deriving (Show)

type Program = [Instruction]

data InterpreterException
    = AtStartOfMemory
    | AtEndOfMemory
    deriving (Show, Typeable)

instance Exception InterpreterException

data Machine = Machine Int (MV.IOVector Int)

getCell :: Machine -> IO Int
getCell (Machine idx mem) = MV.read mem idx

setCell:: Machine -> Int -> IO ()
setCell (Machine idx mem) = MV.write mem idx

parse :: String -> Either String Program
parse s = case go [] (s ++ "]") of
            Left msg        -> Left msg
            Right (p, [])   -> Right (reverse p)
            Right (_, rest) -> Left ("unexpected input: " ++ rest)
  where
    go p ('+':xs) = go (AdjustCell 1 : p) xs
    go p ('-':xs) = go (AdjustCell (-1) : p) xs
    go p ('>':xs) = go (AdjustCellPtr 1 : p) xs
    go p ('<':xs) = go (AdjustCellPtr (-1) : p) xs
    go p ('.':xs) = go (PutChar : p) xs
    go p (',':xs) = go (GetChar : p) xs
    go p ('[':xs) = case go [] xs of
                        Left msg -> Left msg
                        Right (p', rest) -> go (Loop (reverse p') : p) rest
    go p (']':xs) = Right (p, xs)
    go p (_  :xs) = go p xs
    go _ []       = Left "missing ']'"

fuseAdjust :: Program -> Program
fuseAdjust = transformRecursively $ \case
    (AdjustCell x:AdjustCell y:xs) -> Just (AdjustCell (x+y):xs)
    _                              -> Nothing

fuseMove :: Program -> Program
fuseMove = transformRecursively  $ \case
    (AdjustCellPtr x:AdjustCellPtr y:xs) -> Just (AdjustCellPtr (x+y):xs)
    _                                    -> Nothing

dropNullAdjust :: Program -> Program
dropNullAdjust = transformRecursively  $ \case
    (AdjustCell 0:xs) -> Just xs
    _                 -> Nothing

dropNullMove :: Program -> Program
dropNullMove = transformRecursively  $ \case
    (AdjustCellPtr 0:xs) -> Just xs
    _                    -> Nothing

collapse :: Program -> Program
collapse = transform $ \case
    (Loop [Loop x]:xs) -> Just (Loop x:xs)
    _                  -> Nothing

zeroCells :: Program -> Program
zeroCells = transform $ \case
    (Loop [AdjustCell v]:xs)
        | v < 0              -> Just (SetCell 0:xs)
        | otherwise          -> Nothing
    _                        -> Nothing

fuseAssignAndAdjust :: Program -> Program
fuseAssignAndAdjust = transformRecursively $ \case
    (SetCell x:AdjustCell y:xs) -> Just (SetCell (x+y):xs)
    _                           -> Nothing

fuseAssign :: Program -> Program
fuseAssign = transformRecursively $ \case
    (SetCell _:SetCell y:xs) -> Just (SetCell y:xs)
    _                        -> Nothing

transform :: (Program -> Maybe Program) -> Program -> Program
transform trans p@(x:xs) =
    case trans p of
        Just p' -> transform trans p'
        Nothing -> x : transform trans xs
transform _ [] = []

transformRecursively :: (Program -> Maybe Program) -> Program -> Program
transformRecursively trans (Loop p:xs) = Loop (transformRecursively trans p):transformRecursively trans xs
transformRecursively trans p@(x:xs) =
    case trans p of
        Just p' -> transformRecursively trans p'
        Nothing -> x : transformRecursively trans xs
transformRecursively _ [] = []

compile :: String -> Either String Program
compile = either Left (Right . optimize) . parse
  where
    optimize = collapse
             . fuseAssign
             . fuseAssignAndAdjust
             . zeroCells
             . dropNullAdjust
             . fuseAdjust
             . dropNullMove
             . fuseMove

exec :: Machine -> Instruction -> IO Machine
exec m@(Machine idx mem) (AdjustCellPtr v)
    | v > 0 = if idx + v < MV.length mem
                then return (Machine (idx + v) mem)
                else throwIO AtEndOfMemory
    | v < 0 = if idx + v >= 0
                then return (Machine (idx + v) mem)
                else throwIO AtStartOfMemory
    | otherwise = return m
exec m (AdjustCell v) = getCell m >>= setCell m . (+v) >> return m
exec m (SetCell v) = setCell m v >> return m
exec m PutChar = getCell m >>= putChar . chr >> return m
exec m GetChar = getChar >>= setCell m . ord >> return m
exec m l@(Loop p) = do
    curVal <- getCell m
    if curVal /= 0
        then run m p >>= \m' -> exec m' l
        else return m

run :: Machine -> Program -> IO Machine
run = foldM exec

boot :: Int -> IO Machine
boot memSize = Machine 0 <$> MV.replicate memSize 0

