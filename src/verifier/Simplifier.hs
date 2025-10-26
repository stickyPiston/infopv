module Verifier.Simplifier where

import GCLParser.GCLDatatype
import Data.Maybe (fromMaybe)
import ExamplesOfSemanticFunction (freeVariables)

filterIrrelevantConstraints :: Expr Type -> [Expr Type] -> [Expr Type]
filterIrrelevantConstraints for constraints =
    let fvFor = freeVariables for
     in filter (any (`elem` fvFor) . freeVariables) constraints

simplify :: Eq a => Expr a -> Expr a
simplify = \case
    BinopExpr op l r -> case (simplify l, simplify r) of
        (LitI a, LitI b) -> evalIntOp op a b
        (LitB a, b) -> case op of
            And -> if a then b else LitB a
            Or -> if a then LitB a else b
            Implication -> if a then b else LitB True
            _ -> error "Boolean argument passed to non-boolean operator"
        (a, LitB b) -> case op of
            And -> if b then a else LitB b
            Or -> if b then LitB b else a
            Implication -> if b then LitB b else OpNeg a
            _ -> error "Boolean argument passed to non-boolean operator"
        (cfL, cfR) -> BinopExpr op cfL cfR
    OpNeg x -> case simplify x of
        LitB b -> LitB $ not b
        y -> OpNeg y
    Cond i t e -> case simplify i of
        LitB True -> simplify t
        LitB False -> simplify e
        c -> Cond c (simplify t) (simplify e)
    Forall x e -> Forall x $ simplify e
    Exists x e -> Exists x $ simplify e
    SizeOf a -> SizeOf $ simplify a
    RepBy a i e -> RepBy (simplify a) (simplify i) (simplify e)
    ArrayElem a i -> fromMaybe (ArrayElem (simplify a) (simplify i))
        $ checkSpine (simplify i) (simplify a)
    Parens e -> case simplify e of
        LitI i -> LitI i
        LitB b -> LitB b
        cfE -> Parens cfE
    e -> e
    where
        evalIntOp :: BinOp -> Int -> Int -> Expr a
        evalIntOp LessThan l r = LitB $ l < r
        evalIntOp LessThanEqual l r = LitB $ l <= r
        evalIntOp GreaterThan l r = LitB $ l > r
        evalIntOp GreaterThanEqual l r = LitB $ l >= r
        evalIntOp Equal l r = LitB $ l == r
        evalIntOp Alias l r = LitB $ l == r
        evalIntOp Plus l r = LitI $ l + r
        evalIntOp Minus l r = LitI $ l - r
        evalIntOp Multiply l r = LitI $ l * r
        evalIntOp Divide l r = LitI $ l `div` r
        evalIntOp op l r = error $ "Invalid instruction" <> show l <> " " <> show op <> " " <> show r

        checkSpine :: Eq a => Expr a -> Expr a -> Maybe (Expr a)
        checkSpine i (RepBy a i' e)
            | i == i' = Just e
            | otherwise = checkSpine i a
        checkSpine _ _ = Nothing