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
    , singleAccess
    ) where

import Data.Language.Brainfuck.Types

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

type Optimization = Program -> Program

optimize :: Optimization
optimize = dropNullAdjust
         . dropNullMove
         . singleAccess
         . fuseAssign
         . fuseAssignAndAdjust
         . zeroCells
         . collapse
         . fuseAdjust
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

singleAccess :: Optimization
singleAccess [] = []
singleAccess (Loop p:rest) = Loop (singleAccess p) : singleAccess rest
singleAccess program = if null fusableSection
                       then head program : singleAccess (tail program)
                       else fusedSection ++ singleAccess rest
  where
    (fusableSection, rest) = span isFusableInstruction program

    fusedSection = fusedAdjustments ++ [AdjustCellPtr finalPos]

    fusedAdjustments = map (\adj -> AdjustCellAt (fst (head adj)) (sum (map snd adj)))
                     . groupBy ((==) `on` fst)
                     . sortBy (comparing fst)
                     $ adjustments

    (finalPos, adjustments) = foldl go (0, []) fusableSection
      where
        go (pos, upd) (AdjustCellPtr v)  = (pos + v, upd)
        go (pos, upd) (AdjustCellAt p v) = (pos, (pos + p, v):upd)
        go _          _                  = error "isFusableInstruction wrong?"

    isFusableInstruction (AdjustCellPtr _)  = True
    isFusableInstruction (AdjustCellAt _ _) = True
    isFusableInstruction _                  = False

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

