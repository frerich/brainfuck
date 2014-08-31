import Data.Language.Brainfuck

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
            [] -> getContents
            (fn:_) -> readFile fn
    case compile code of
        Left errorMsg -> putStr errorMsg >> exitFailure
        Right program -> boot 65536 program
