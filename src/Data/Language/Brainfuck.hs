module Data.Language.Brainfuck
    ( boot
    , compile
    , run

    , Program
    , Machine()
    , InterpreterException(..)
    )
where

import Data.Language.Brainfuck.Interpreter
import Data.Language.Brainfuck.Optimizer
import Data.Language.Brainfuck.Parser
import Data.Language.Brainfuck.Types

compile :: String -> Either String Program
compile = fmap optimize . parse


