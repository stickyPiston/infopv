module Verifier.Simplifier where

import GCLParser.GCLDatatype
import Data.Maybe (fromMaybe)
import ExamplesOfSemanticFunction (freeVariables)

simplify :: Eq a => Expr a -> Expr a
simplify = simplifyAssignAssgns

filterIrrelevantConstraints :: Expr Type -> [Expr Type] -> [Expr Type]
filterIrrelevantConstraints for constraints =
    let fvFor = freeVariables for
     in filter (any (`elem` fvFor) . freeVariables) constraints

simplifyAssignAssgns :: Eq a => Expr a -> Expr a
simplifyAssignAssgns = \case
    e@(ArrayElem a i) -> fromMaybe e $ checkSpine i a
    OpNeg e -> OpNeg $ simplifyAssignAssgns e
    BinopExpr op l r -> BinopExpr op (simplifyAssignAssgns l) (simplifyAssignAssgns r)
    Forall x e -> Forall x $ simplifyAssignAssgns e
    Exists x e -> Exists x $ simplifyAssignAssgns e
    SizeOf a -> SizeOf $ simplifyAssignAssgns a
    RepBy a i e -> RepBy (simplifyAssignAssgns a) (simplifyAssignAssgns i) (simplifyAssignAssgns e)
    Cond i t e -> Cond (simplifyAssignAssgns i) (simplifyAssignAssgns t) (simplifyAssignAssgns e)
    e -> e
    where
        checkSpine :: Eq a => Expr a -> Expr a -> Maybe (Expr a)
        checkSpine i (RepBy a i' e)
            | i == i' = Just e
            | otherwise = checkSpine i a
        checkSpine _ _ = Nothing