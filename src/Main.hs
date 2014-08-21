{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (throwIO, Exception, catch)
import Control.Monad (foldM, void)
import Data.Char (ord, chr)
import Data.Typeable (Typeable)
import Data.Label (mkLabels, get, modify)
import Data.List.Zipper
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

data Instruction
    = AdjustCell Int
    | AdjustCellPtr Int
    | PutChar
    | GetChar
    | Loop Program
    deriving (Show)

data InterpreterException
    = AtStartOfMemory
    | AtEndOfMemory
    deriving (Show, Typeable)

instance Exception InterpreterException

type Program = [Instruction]

data Machine = Machine { _memory :: Zipper Int }
mkLabels [''Machine]

compose :: [a -> a] -> a -> a
compose = foldr (.) id

adjust :: (a -> a) -> Zipper a -> Zipper a
adjust f z = replace (f . cursor $ z) z

compile :: String -> Either String Program
compile = go [] []
  where
    go :: [Instruction] -> [[Instruction]] -> String -> Either String Program
    go p stack s@(x:xs)
        | x `elem` "+-" = let (v, rest) = compileSpan '+' '-' s in go (AdjustCell v : p) stack rest
        | x `elem` "><" = let (v, rest) = compileSpan '>' '<' s in go (AdjustCellPtr v : p) stack rest
        | x == '.'  = go (PutChar : p) stack xs
        | x == ','  = go (GetChar : p) stack xs
        | x == '['  = go [] (p:stack) xs
        | x == ']'  = case stack of
                        (a:as) -> go (Loop (reverse p) : a) as xs
                        _      -> Left "unexpected ']'"
        | otherwise = go p stack xs
    go p [] [] = Right (reverse p)
    go _ _  [] = Left "unexpected EOI, ']' missing"

    compileSpan :: Char -> Char -> String -> (Int, String)
    compileSpan inc dec str = (sum (map (\x -> if x == inc then 1 else (-1)) as), bs)
      where
        (as, bs) = span (`elem` [inc, dec]) str

distLeft :: Zipper a -> Int
distLeft (Zip l _) = length l

distRight :: Zipper a -> Int
distRight (Zip _ r) = length r

exec :: Machine -> Instruction -> IO Machine
exec machine i = handle i machine
  where
    handle (AdjustCellPtr v) m
        | v > 0 = if distRight (get memory m) >= v
                    then return (modify memory (compose (replicate v right)) m)
                    else throwIO AtEndOfMemory
        | v < 0 = if distLeft (get memory m) >= (-v)
                    then return (modify memory (compose (replicate (-v) left)) m)
                    else throwIO AtStartOfMemory
        | otherwise = return m
    handle (AdjustCell v) m = return (modify memory (adjust (+v)) m)
    handle PutChar m = putChar (chr . cursor . get memory $ m) >> return m
    handle GetChar m = getChar >>= \c -> return (modify memory (replace . ord $ c) m)
    handle l@(Loop p) m =
        if cursor (get memory m) /= 0
            then run m p >>= handle l
            else return m

run :: Machine -> Program -> IO Machine
run = foldM exec

boot :: Int -> Program -> IO ()
boot memSize program = do
    hSetBuffering stdout NoBuffering
    let m = Machine (fromList (replicate memSize 0))
    catch (void (run m program)) $ \e -> do
        putStr "... ERROR! "
        case e of
            AtStartOfMemory -> putStrLn "Cannot move left: already at start of memory."
            AtEndOfMemory -> putStrLn "Cannot move right: already at end of memory."
        exitFailure
    return ()

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
            [] -> getContents
            (fn:_) -> readFile fn
    case compile code of
        Left errorMsg -> putStr errorMsg >> exitFailure
        Right program -> boot 65536 program
