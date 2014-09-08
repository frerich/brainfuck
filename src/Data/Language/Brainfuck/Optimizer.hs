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
    (AdjustCellAt p x:AdjustCellAt q y:xs)
        | p == q    -> Just (AdjustCellAt p (x+y):xs)
        | otherwise -> Nothing
    _               -> Nothing

fuseMove :: Optimization
fuseMove = transformRecursively  $ \case
    (AdjustCellPtr x:AdjustCellPtr y:xs) -> Just (AdjustCellPtr (x+y):xs)
    _                                    -> Nothing

dropNullAdjust :: Optimization
dropNullAdjust = transformRecursively  $ \case
    (AdjustCellAt _ 0:xs) -> Just xs
    _                     -> Nothing

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
    (Loop [AdjustCellAt 0 v]:xs)
        | v < 0     -> Just (SetCellAt 0 0:xs)
        | otherwise -> Nothing
    _               -> Nothing

fuseAssignAndAdjust :: Optimization
fuseAssignAndAdjust = transformRecursively $ \case
    (SetCellAt p x:AdjustCellAt q y:xs)
        | p == q    -> Just (SetCellAt p (x+y):xs)
        | otherwise -> Nothing
    _               -> Nothing

fuseAssign :: Optimization
fuseAssign = transformRecursively $ \case
    (SetCellAt p _:SetCellAt q y:xs)
        | p == q    -> Just (SetCellAt p y:xs)
        | otherwise -> Nothing
    _               -> Nothing

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

