module Data.Language.Brainfuck.Types where

data Instruction
    = AdjustCellAt Int Int
    | AdjustCellPtr Int
    | PutChar
    | GetChar
    | Loop Program
    | SetCellAt Int Int
    deriving (Show)

type Program = [Instruction]


