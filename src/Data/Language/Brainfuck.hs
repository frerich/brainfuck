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
parse = go [] []
  where
    go :: [Instruction] -> [[Instruction]] -> String -> Either String Program
    go p stack (x:xs)
        | x == '+' = go (AdjustCell 1 : p) stack xs
        | x == '-' = go (AdjustCell (-1) : p) stack xs
        | x == '>' = go (AdjustCellPtr 1 : p) stack xs
        | x == '<' = go (AdjustCellPtr (-1) : p) stack xs
        | x == '.'  = go (PutChar : p) stack xs
        | x == ','  = go (GetChar : p) stack xs
        | x == '['  = go [] (p:stack) xs
        | x == ']'  = case stack of
                        (a:as) -> go (Loop (reverse p) : a) as xs
                        _      -> Left "unexpected ']'"
        | otherwise = go p stack xs
    go p [] [] = Right (reverse p)
    go _ _  [] = Left "unexpected EOI, ']' missing"

merge :: Program -> Program
merge (AdjustCell x:AdjustCell y:rest)       = merge (AdjustCell (x+y) : rest)
merge (AdjustCellPtr x:AdjustCellPtr y:rest) = merge (AdjustCellPtr (x+y) : rest)
merge (Loop p:rest)                          = Loop (merge p) : merge rest
merge (x:xs)                                 = x : merge xs
merge []                                     = []

compile :: String -> Either String Program
compile = either Left (Right . merge) . parse

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

