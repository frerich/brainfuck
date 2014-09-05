module Data.Language.Brainfuck.Parser where

import Data.Language.Brainfuck.Types

parse :: String -> Either String Program
parse s = case go [] (s ++ "]") of
            Left msg        -> Left msg
            Right (p, [])   -> Right (reverse p)
            Right (_, rest) -> Left ("unexpected input: " ++ rest)
  where
    go p ('+':xs) = go (AdjustCell 1 : p) xs
    go p ('-':xs) = go (AdjustCell (-1) : p) xs
    go p ('>':xs) = go (AdjustCellPtr 1 : p) xs
    go p ('<':xs) = go (AdjustCellPtr (-1) : p) xs
    go p ('.':xs) = go (PutChar : p) xs
    go p (',':xs) = go (GetChar : p) xs
    go p ('[':xs) = case go [] xs of
                        Left msg -> Left msg
                        Right (p', rest) -> go (Loop (reverse p') : p) rest
    go p (']':xs) = Right (p, xs)
    go p (_  :xs) = go p xs
    go _ []       = Left "missing ']'"
