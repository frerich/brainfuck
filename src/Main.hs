{-# LANGUAGE LambdaCase #-}

import Data.Language.Brainfuck

import Control.Exception (catch)
import Control.Monad (void)
import System.Environment (getArgs)
import System.Exit (exitFailure)

execute :: Program -> IO ()
execute program = do
    m <- boot 65536
    catch (void (run m program)) $ \e -> do
        putStr "... ERROR! "
        case e of
            AtStartOfMemory -> putStrLn "Cannot move left: already at start of memory."
            AtEndOfMemory -> putStrLn "Cannot move right: already at end of memory."
        exitFailure
    return ()

main :: IO ()
main = do
    code <- getArgs >>= \case
                [] -> getContents
                (fn:_) -> readFile fn
    case compile code of
        Left errorMsg -> putStr errorMsg >> exitFailure
        Right program -> print program >> execute program
