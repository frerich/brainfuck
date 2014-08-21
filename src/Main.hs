{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Vector.Mutable as MV
import Control.Exception (throwIO, Exception, catch)
import Control.Monad (foldM, void)
import Data.Char (ord, chr)
import Data.Functor ((<$>))
import Data.Typeable (Typeable)
import Data.Label (mkLabels, get, modify)
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

type Program = [Instruction]

data InterpreterException
    = AtStartOfMemory
    | AtEndOfMemory
    deriving (Show, Typeable)

instance Exception InterpreterException

data Machine = Machine
    { _memoryIdx :: Int
    , _memory :: MV.IOVector Int
    }

mkLabels [''Machine]

getCell :: Machine -> IO Int
getCell m = MV.read (get memory m) (get memoryIdx m)

setCell:: Machine -> Int -> IO ()
setCell m = MV.write (get memory m) (get memoryIdx m)

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

exec :: Machine -> Instruction -> IO Machine
exec m (AdjustCellPtr v)
    | v > 0 = if get memoryIdx m + v < MV.length (get memory m)
                then return (modify memoryIdx (+v) m)
                else throwIO AtEndOfMemory
    | v < 0 = if get memoryIdx m + v >= 0
                then return (modify memoryIdx (+v) m)
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

boot :: Int -> Program -> IO ()
boot memSize program = do
    hSetBuffering stdout NoBuffering
    m <- Machine 0 <$> MV.replicate memSize 0
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
