{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Vector as V
import Control.Exception (throwIO, Exception, catch)
import Control.Monad (unless)
import Data.Functor ((<$>))
import Data.Char (ord, chr)
import Data.Typeable (Typeable)
import Data.Label (mkLabels, get, set, modify)
import Data.List.Zipper
import System.Exit (exitFailure)
import System.Environment (getArgs)

data Machine = Machine
    { _code :: V.Vector Char
    , _codeIdx :: Int
    , _memory :: Zipper Int
    }

mkLabels [''Machine]

data InterpreterException = AtStartOfMemory
                          | AtEndOfMemory
                          | NoLoopStart
                          | NoLoopEnd
    deriving (Show, Typeable)

instance Exception InterpreterException

adjust :: (a -> a) -> Zipper a -> Zipper a
adjust f z = replace (f . cursor $ z) z

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
exec ch = handle ch . modify codeIdx (+1)
  where
    handle '>' m = execMemIdxShift (not . endp . get memory) AtEndOfMemory right m
    handle '<' m = execMemIdxShift (not . beginp . get memory) AtStartOfMemory left m
    handle '+' m = return (modify memory (adjust (+1)) m)
    handle '-' m = return (modify memory (adjust (subtract 1)) m)
    handle '.' m = putChar (chr . cursor . get memory $ m) >> return m
    handle ',' m = do
        c <- getChar
        return (modify memory (replace . ord $ c) m)
    handle '[' m = execJump (== 0) findClosingBracket NoLoopEnd m
    handle ']' m = execJump (/= 0) findOpeningBracket NoLoopStart m
    handle _   m = return m

    execMemIdxShift p errorType idxAdjustment m
        | p m       = return (modify memory idxAdjustment m)
        | otherwise = throwIO errorType

    execJump jumpCond locator errorType m = 
        if not . jumpCond . cursor . get memory $ m then return m else
            case locator (get code m) (get codeIdx m - 1) of
                Just pos -> return (set codeIdx (pos + 1) m)
                Nothing  -> throwIO errorType

boot :: Int -> String -> Machine
boot memSize program =
    Machine (V.fromList program) 0 (fromList (replicate memSize 0))

run :: Machine -> IO ()
run m = unless (i >= V.length c) (exec (c V.! i) m >>= run)
  where
    c = get code m
    i = get codeIdx m

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

