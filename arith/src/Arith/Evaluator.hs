module Arith.Evaluator (eval1, eval, eval') where

import Arith.Syntax
import Control.Monad

eval1 :: Term -> Maybe Term
eval1 TermTrue                         = Nothing
eval1 TermFalse                        = Nothing
eval1 (TermIf TermTrue  t2 _ )         = return t2
eval1 (TermIf TermFalse _  t3)         = return t3
eval1 (TermIf t1        t2 t3)         = liftM3 TermIf (eval1 t1) (return t2) (return t3)
eval1 TermZero                         = Nothing
eval1 (TermSucc t)                     = liftM TermSucc (eval1 t)
eval1 (TermPred TermZero)              = return TermZero
eval1 (TermPred (TermSucc t))          = return t
eval1 (TermPred t)                     = liftM TermPred (eval1 t)
eval1 (TermIsZero TermZero)            = return TermTrue
eval1 (TermIsZero (TermSucc TermZero)) = return TermFalse
eval1 (TermIsZero t)                   = liftM TermIsZero (eval1 t)

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

{- Types and Programming Languages Exercise 4.2.2
    Big-step style evaluation; note: without numeric value checking -}
eval' :: Term -> Term
eval' t = case t of
            TermTrue          -> TermTrue
            TermFalse         -> TermFalse
            TermZero          -> TermZero
            (TermIf t1 t2 t3) -> case eval' t1 of
                                   TermTrue  -> eval' t2
                                   TermFalse -> eval' t3
                                   _         -> t
            (TermSucc t1)     -> TermSucc (eval' t1)
            (TermPred t1)     -> case eval' t1 of
                                   TermZero       -> TermZero
                                   (TermSucc nv1) -> nv1
                                   _              -> t
            (TermIsZero t1)   -> case eval' t1 of
                                   TermZero     -> TermTrue
                                   (TermSucc _) -> TermFalse
                                   _            -> t

