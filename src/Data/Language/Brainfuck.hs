{-# LANGUAGE DeriveDataTypeable #-}

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

merge :: Program -> Program
merge (AdjustCell x:AdjustCell y:rest)
    | x + y /= 0 = merge (AdjustCell (x+y) : rest)
    | otherwise  = merge rest
merge (AdjustCellPtr x:AdjustCellPtr y:rest)
    | x + y /= 0 = merge (AdjustCellPtr (x+y) : rest)
    | otherwise  = merge rest
merge (Loop p:rest) = Loop (merge p) : merge rest
merge (x:xs)        = x : merge xs
merge []            = []

collapse :: Program -> Program
collapse (Loop [Loop x]:rest) = collapse (Loop x:rest)
collapse (x:xs)               = x : collapse xs
collapse []                   = []

zeroCells :: Program -> Program
zeroCells (Loop [AdjustCell v]:rest)
    | v < 0     = SetCell 0 : zeroCells rest
    | otherwise = Loop [AdjustCell v] : zeroCells rest
zeroCells (Loop p:rest) = Loop (zeroCells p) : zeroCells rest
zeroCells (x:xs)        = x : zeroCells xs
zeroCells []            = []

fuseAssignAndAdjust :: Program -> Program
fuseAssignAndAdjust (SetCell x:AdjustCell y:rest) = fuseAssignAndAdjust (SetCell (x+y) : rest)
fuseAssignAndAdjust (Loop p:rest)                 = Loop (fuseAssignAndAdjust p) : fuseAssignAndAdjust rest
fuseAssignAndAdjust (x:xs)                        = x : fuseAssignAndAdjust xs
fuseAssignAndAdjust []                            = []

fuseAssign :: Program -> Program
fuseAssign (SetCell _:SetCell y:rest) = fuseAssign (SetCell y:rest)
fuseAssign (Loop p:rest)              = Loop (fuseAssign p) : fuseAssign rest
fuseAssign (x:xs)                     = x : fuseAssign xs
fuseAssign []                         = []

compile :: String -> Either String Program
compile = either Left (Right . optimize) . parse
  where
    optimize = collapse
             . fuseAssign
             . fuseAssignAndAdjust
             . zeroCells
             . merge

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

