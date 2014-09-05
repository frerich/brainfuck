module Data.Language.Brainfuck.Types where

data Instruction
    = AdjustCell Int
    | AdjustCellPtr Int
    | PutChar
    | GetChar
    | Loop Program
    | SetCell Int
    deriving (Show)

type Program = [Instruction]


