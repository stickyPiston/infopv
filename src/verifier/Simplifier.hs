module Verifier.Simplifier where

import GCLParser.GCLDatatype
import Data.Maybe (fromMaybe)

simplify :: Eq a => Expr a -> Expr a
simplify = \case
    BinopExpr op l r -> case (simplify l, simplify r) of
        (a, b) | op == Equal && a==b -> LitB True 
               | op == Equal -> BinopExpr op a b
        (LitI a, LitI b) -> evalIntOp op a b
        (LitB a, b) -> case op of
            And -> if a then b else LitB a
            Or -> if a then LitB a else b
            Implication -> if a then b else LitB True
            x -> error $ "Boolean argument passed to non-boolean operator: " ++ show a ++ show x ++ show b
        (a, LitB b) -> case op of
            And -> if b then a else LitB b
            Or -> if b then LitB b else a
            Implication -> if b then LitB b else OpNeg a
            x -> error $ "Boolean argument passed to non-boolean operator: " ++ show a ++ show x ++ show b
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
    ArrayElem a i -> case simplify i of
        LitI n -> case reduceRepbySpine n (simplify a) of
            Left e -> e
            Right a' -> ArrayElem a' (LitI n)
        simpleI -> fromMaybe (ArrayElem (simplify a) simpleI) $ checkSpine simpleI (simplify a)
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

        reduceRepbySpine :: Eq a => Int -> Expr a -> Either (Expr a) (Expr a)
        reduceRepbySpine i (RepBy a (LitI i') e)
            | i == i' = Left e
            | otherwise = reduceRepbySpine i a
        reduceRepbySpine i (RepBy a i' e) = (\a' -> RepBy a' i' e) <$> reduceRepbySpine i a  
        reduceRepbySpine _ e = Right e

        checkSpine :: Eq a => Expr a -> Expr a -> Maybe (Expr a)
        checkSpine i (RepBy a i' e)
            | i == i' = Just e
            | otherwise = checkSpine i a
        checkSpine _ _ = Nothing