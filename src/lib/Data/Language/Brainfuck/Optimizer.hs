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
optimize = foldr (.) id . map foldProgram $
         [ dropNullAdjust
         , dropNullMove
         , singleAccess
         , fuseAssign
         , fuseAssignAndAdjust
         , zeroCells
         , collapse
         , fuseAdjust
         , fuseMove
         ]

fuseAdjust :: Optimization
fuseAdjust = foldr go []
  where
    go i@(AdjustCellAt p x) p'@(AdjustCellAt q y:is)
        | p == q    = AdjustCellAt p (x+y) : is
        | otherwise = i : p'
    go i p'         = i : p'

fuseMove :: Optimization
fuseMove = foldr go []
  where
    go (AdjustCellPtr x) (AdjustCellPtr y:is) = AdjustCellPtr (x+y) : is
    go i p'                                   = i : p'

dropNullAdjust :: Optimization
dropNullAdjust = filter (not . nullAdjust)
  where
    nullAdjust (AdjustCellAt _ 0) = True
    nullAdjust _                  = False

dropNullMove :: Optimization
dropNullMove = filter (not . nullMove)
  where
    nullMove (AdjustCellPtr 0) = True
    nullMove _                 = False

collapse :: Optimization
collapse = map $ \case
    Loop [Loop p] -> Loop p
    x             -> x

zeroCells :: Optimization
zeroCells = map $ \case
    Loop [AdjustCellAt 0 (-1)] -> SetCellAt 0 0
    x                          -> x

fuseAssignAndAdjust :: Optimization
fuseAssignAndAdjust = foldr go []
  where
    go i@(SetCellAt p x) p'@(AdjustCellAt q y:is)
        | p == q    = SetCellAt p (x+y) : is
        | otherwise = i : p'
    go i p'         = i : p'

fuseAssign :: Optimization
fuseAssign = foldr go []
  where
    go i@(SetCellAt p _) p'@(SetCellAt q _:_)
        | p == q    = p'
        | otherwise = i : p'
    go i p'         = i : p'

singleAccess :: Optimization
singleAccess [] = []
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

foldProgram :: (Program -> Program) -> Program -> Program
foldProgram f = f . foldr go []
  where
    go (Loop x) p' = Loop (foldProgram f x) : p'
    go x        p' = x : p'

