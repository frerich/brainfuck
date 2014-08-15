{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.Vector as V
import Control.Exception (throwIO, Exception, catch)
import Control.Monad (unless)
import Data.Functor ((<$>))
import Data.Char (ord, chr)
import Data.Typeable (Typeable)
import Data.List.Zipper
import System.Exit (exitFailure)
import System.Environment (getArgs)

data Machine = Machine (V.Vector Char) Int (Zipper Int)

data InterpreterException = AtStartOfMemory
                          | AtEndOfMemory
                          | NoLoopStart
                          | NoLoopEnd
    deriving (Show, Typeable)

instance Exception InterpreterException

writeCell :: Machine -> Int -> Machine
writeCell (Machine code codeIdx mem) v = Machine code codeIdx (replace v mem)

readCell :: Machine -> Int
readCell (Machine _ _ mem) = cursor mem

editCell :: (Int -> Int) -> Machine -> Machine
editCell f m = writeCell m (f (readCell m))

updateCodeIdx :: (Int -> Int) -> Machine -> Machine
updateCodeIdx f (Machine code codeIdx mem) = Machine code (f codeIdx) mem

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
    handle '>' m = execMemIdxShift (\m@(Machine _ _ mem) -> not (endp mem)) AtEndOfMemory right m
    handle '<' m = execMemIdxShift (\m@(Machine _ _ mem) -> not (beginp mem)) AtStartOfMemory left m
    handle '+' m = return (editCell (+1) m)
    handle '-' m = return (editCell (subtract 1) m)
    handle '.' m = putChar (chr (readCell m)) >> return m
    handle ',' m = getChar >>= return . writeCell m . ord
    handle '[' m = execJump (== 0) findClosingBracket NoLoopEnd m
    handle ']' m = execJump (/= 0) findOpeningBracket NoLoopStart m
    handle _   m = return m

    execMemIdxShift p errorType adjust m@(Machine code codeIdx mem)
        | p m       = return (Machine code codeIdx (adjust mem))
        | otherwise = throwIO errorType

    execJump jumpCond locator errorType m@(Machine code codeIdx mem) =
        if not (jumpCond (readCell m)) then return m else
            case locator code (codeIdx - 1) of
                Just matchingBracketPos -> return (Machine code (matchingBracketPos + 1) mem)
                Nothing                 -> throwIO errorType

boot :: Int -> String -> Machine
boot memSize program =
    Machine (V.fromList program) 0 (fromList (replicate memSize 0))

run :: Machine -> IO ()
run m@(Machine code codePtr _) =
    unless (codePtr >= V.length code) $
        exec (code V.! codePtr) m >>= run

main :: IO ()
main = do
    args <- getArgs
    program <- case args of
                    [] -> getContents
                    (fn:_) -> readFile fn
    let machine = boot 65536 program
    catch (run machine) $ \e -> do
        putStr "... -- ERROR! "
        case e of
            AtStartOfMemory -> putStr "Cannot move left: already at start of memory."
            AtEndOfMemory -> putStr "Cannot move right: already at end of memory."
            NoLoopStart -> putStr "Cannot restart loop: loop beginning not found."
            NoLoopEnd -> putStr "Cannot exit loop: loop end not found."
        exitFailure
    putChar '\n'

