{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Exception (throwIO, Exception, catch)
import Control.Monad (unless)
import Data.Functor ((<$>))
import Data.Char (ord, chr)
import Data.Typeable (Typeable)
import System.Exit (exitFailure)
import System.Environment (getArgs)

data Machine = Machine (V.Vector Char) Int (MV.IOVector Int) Int

data InterpreterException = AtStartOfMemory
                          | AtEndOfMemory
                          | NoLoopStart
                          | NoLoopEnd
    deriving (Show, Typeable)

instance Exception InterpreterException

writeCell :: Machine -> Int -> IO ()
writeCell (Machine _ _ mem ptr) = MV.write mem ptr

readCell :: Machine -> IO Int
readCell (Machine _ _ mem ptr) = MV.read mem ptr

editCell :: (Int -> Int) -> Machine -> IO Machine
editCell f m = f <$> readCell m >>= writeCell m >> return m

editCellIdx :: (Int -> Int) -> Machine -> Machine
editCellIdx f (Machine code codeIdx mem memIdx) = Machine code codeIdx mem (f memIdx)

updateCodeIdx :: (Int -> Int) -> Machine -> Machine
updateCodeIdx f (Machine code codeIdx mem memIdx) = Machine code (f codeIdx) mem memIdx

findMatchingBracket :: Char -> Char -> (Int -> Int) -> V.Vector Char -> Int -> Maybe Int
findMatchingBracket closeCh openCh step v i
    | i >= 0 && i < V.length v =
        if v V.! i == closeCh
            then Just i
            else
                if v V.! i == openCh
                    then step <$> findMatchingBracket closeCh openCh step v (step i) >>=
                                  findMatchingBracket closeCh openCh step v
                    else findMatchingBracket closeCh openCh step v (step i)
    | otherwise = Nothing

findClosingBracket :: V.Vector Char -> Int -> Maybe Int
findClosingBracket v idx = findMatchingBracket ']' '[' (+1) v (idx + 1)

findOpeningBracket :: V.Vector Char -> Int -> Maybe Int
findOpeningBracket v idx = findMatchingBracket '[' ']' (subtract 1) v (idx - 1)

exec :: Char -> Machine -> IO Machine
exec ch = handle ch . updateCodeIdx (+1)
  where
    handle '>' m@(Machine _ _ mem memIdx)
        | memIdx < MV.length mem = return (editCellIdx (+1) m)
        | otherwise              = throwIO AtEndOfMemory
    handle '<' m@(Machine _ _ _ memIdx)
        | memIdx > 0 = return (editCellIdx (subtract 1) m)
        | otherwise  = throwIO AtStartOfMemory
    handle '+' m = editCell (+1) m
    handle '-' m = editCell (subtract 1) m
    handle '.' m = chr <$> readCell m >>= putChar >> return m
    handle ',' m = ord <$> getChar >>= writeCell m >> return m
    handle '[' m = execJump (== 0) findClosingBracket NoLoopEnd m
    handle ']' m = execJump (/= 0) findOpeningBracket NoLoopStart m
    handle _   m = return m

    execJump jumpCond locator errorType m@(Machine code codeIdx mem memIdx) = do
        cell <- readCell m
        if not (jumpCond cell) then return m else
            case locator code (codeIdx - 1) of
                Just matchingBracketPos -> return (Machine code (matchingBracketPos + 1) mem memIdx)
                Nothing                 -> throwIO errorType

boot :: Int -> String -> IO Machine
boot memSize program = do
    mem <- MV.replicate memSize 0
    return (Machine (V.fromList program) 0 mem 0)

run :: Machine -> IO ()
run m@(Machine code codePtr _ _) =
    unless (codePtr >= V.length code) $
        exec (code V.! codePtr) m >>= run

main :: IO ()
main = do
    args <- getArgs
    program <- case args of
                    [] -> getContents
                    (fn:_) -> readFile fn
    machine <- boot 65536 program
    catch (run machine) $ \e -> do
        putStr "... -- ERROR! "
        case e of
            AtStartOfMemory -> putStr "Cannot move left: already at start of memory."
            AtEndOfMemory -> putStr "Cannot move right: already at end of memory."
            NoLoopStart -> putStr "Cannot restart loop: loop beginning not found."
            NoLoopEnd -> putStr "Cannot exit loop: loop end not found."
        exitFailure
    putChar '\n'

