{-# LANGUAGE LambdaCase #-}

module Data.Language.Brainfuck.Optimizer
    ( optimize

    , fuseAdjust
    , fuseMove
    , fuseAssignAndAdjust
    , fuseAssign

    , dropNullAdjust
    , dropNullMove

    , collapse
    , zeroCells
    ) where

import Data.Language.Brainfuck.Types

type Optimization = Program -> Program

optimize :: Optimization
optimize = fuseAssign
         . fuseAssignAndAdjust
         . zeroCells
         . collapse
         . dropNullAdjust
         . fuseAdjust
         . dropNullMove
         . fuseMove

fuseAdjust :: Optimization
fuseAdjust = transformRecursively $ \case
    (AdjustCell x:AdjustCell y:xs) -> Just (AdjustCell (x+y):xs)
    _                              -> Nothing

fuseMove :: Optimization
fuseMove = transformRecursively  $ \case
    (AdjustCellPtr x:AdjustCellPtr y:xs) -> Just (AdjustCellPtr (x+y):xs)
    _                                    -> Nothing

dropNullAdjust :: Optimization
dropNullAdjust = transformRecursively  $ \case
    (AdjustCell 0:xs) -> Just xs
    _                 -> Nothing

dropNullMove :: Optimization
dropNullMove = transformRecursively  $ \case
    (AdjustCellPtr 0:xs) -> Just xs
    _                    -> Nothing

collapse :: Optimization
collapse = transform $ \case
    (Loop [Loop x]:xs) -> Just (Loop x:xs)
    _                  -> Nothing

zeroCells :: Optimization
zeroCells = transform $ \case
    (Loop [AdjustCell v]:xs)
        | v < 0              -> Just (SetCell 0:xs)
        | otherwise          -> Nothing
    _                        -> Nothing

fuseAssignAndAdjust :: Optimization
fuseAssignAndAdjust = transformRecursively $ \case
    (SetCell x:AdjustCell y:xs) -> Just (SetCell (x+y):xs)
    _                           -> Nothing

fuseAssign :: Optimization
fuseAssign = transformRecursively $ \case
    (SetCell _:SetCell y:xs) -> Just (SetCell y:xs)
    _                        -> Nothing

transform :: (Program -> Maybe Program) -> Optimization
transform trans p@(x:xs) =
    case trans p of
        Just p' -> transform trans p'
        Nothing -> x : transform trans xs
transform _ [] = []

transformRecursively :: (Program -> Maybe Program) -> Optimization
transformRecursively trans (Loop p:xs) = Loop (transformRecursively trans p):transformRecursively trans xs
transformRecursively trans p@(x:xs) =
    case trans p of
        Just p' -> transformRecursively trans p'
        Nothing -> x : transformRecursively trans xs
transformRecursively _ [] = []

