{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import GCLUtils
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype

import System.Environment (getArgs)
import Z3.Monad as Z3 hiding (local)
import Control.Monad
import ExamplesOfSemanticFunction
import Data.Maybe
import Control.Monad.Reader
import Debug.Trace

main :: IO ()
main = getArgs >>= \case
    [] -> return ()
    [filepath] -> parseGCLfile filepath >>= \case
        Left err -> putStrLn $ "Could not parse " <> filepath <> ": " <> err
        Right Program { stmt, input, output } -> do
            let initialGamma = [(name, ty) | VarDeclaration name ty <- input ++ output]
            let pre = runReader (wlp stmt (LitB True)) initialGamma
            solverRes <- evalZ3 do
                assert =<< mkNot =<< fromExpr pre
                traceM =<< solverToString
                check
            case solverRes of
                Sat -> putStrLn "Program invalid"
                Unsat -> putStrLn "Program valid"

type Typed = Type
type Untyped = ()
type Predicate = Expr

type Gamma = [(String, Type)]

wlp :: Stmt Untyped -> Predicate Typed -> Reader Gamma (Predicate Typed)
wlp stmt q = case stmt of
    Skip               -> return q
    Assert p           -> (`opAnd` q) <$> annotatePredicate p
    Assume p           -> (`opImplication` q) <$> annotatePredicate p
    Assign var e       -> do
        ae <- annotatePredicate e
        return $ (var |-> ae) q
    Seq s1 s2          -> wlp s1 =<< wlp s2 q
    IfThenElse g s1 s2 -> do
        l <- liftM2 opImplication (annotatePredicate g) (wlp s1 q)
        r <- liftM2 opImplication (OpNeg <$> annotatePredicate g) (wlp s2 q)
        return $ l `opAnd` r
    While gaurd s      -> undefined
    Block vars s       -> local ([(name, ty) | VarDeclaration name ty <- vars] ++) do
        base <- wlp s q
        return $ foldr Forall base [name | VarDeclaration name _ <- vars]

annotatePredicate :: Predicate Untyped -> Reader Gamma (Predicate Typed)
annotatePredicate = \case
    Var name _ -> Var name <$> asks (fromJust . lookup name)
    Parens inner -> Parens <$> annotatePredicate inner
    OpNeg rand -> OpNeg <$> annotatePredicate rand
    BinopExpr binop l r -> BinopExpr binop <$> annotatePredicate l <*> annotatePredicate r
    Forall name body -> Forall name <$> local ((name, PType PTInt) :) (annotatePredicate body)
    Exists name body -> Exists name <$> local ((name, PType PTInt) :) (annotatePredicate body)
    Cond cond then_ else_ -> liftM3 Cond
        (annotatePredicate cond)
        (annotatePredicate then_)
        (annotatePredicate else_)
    LitI n -> return $ LitI n
    LitB b -> return $ LitB b

(|->) :: String -> Expr a -> Expr a -> Expr a
(x |-> for) in_ = case in_ of
    Var name _
        | x == name -> for
        | otherwise -> in_
    Parens inner -> Parens $ (x |-> for) inner
    OpNeg inner -> OpNeg $ (x |-> for) inner
    BinopExpr binop l r -> BinopExpr binop
        ((x |-> for) l)
        ((x |-> for) r)
    Forall name body
        | x == name -> in_
        | otherwise -> Forall name $ (x |-> for) body
    Exists name body
        | x == name -> in_
        | otherwise -> Exists name $ (x |-> for) body
    Cond cond then_ else_ -> Cond
        ((x |-> for) cond)
        ((x |-> for) then_)
        ((x |-> for) else_)
    _ -> in_

fromType :: Type -> Z3 Z3.Sort
fromType = \case
    PType PTInt -> mkIntSort
    PType PTBool -> mkBoolSort
    RefType -> undefined
    AType ty -> join $ mkArraySort <$> mkIntSort <*> fromType (PType ty)

fromExpr :: Expr Type -> Z3 Z3.AST
fromExpr e = do
    env <- forM (freeVariables e) \ name -> do
        z3var <- mkStringSymbol name
        return (name, z3var)
    go env e
    where
        go :: [(String, Z3.Symbol)] -> Expr Type -> Z3 Z3.AST
        go env = \case
            (Var var ty)             -> do
                sort <- fromType ty
                mkVar (fromJust $ lookup var env) sort
            (LitI x)              -> mkInt x =<< mkIntSort
            (LitB b)           -> mkBool b
            -- LitNull               -> _
            -- (Dereference u)       -> _
            (Parens e)            -> go env e
            -- (ArrayElem var index) -> _
            (OpNeg expr)          -> mkNot =<< go env expr
            (BinopExpr op e1 e2)  -> do
                lhs <- go env e1
                rhs <- go env e2
                case op of
                    And -> mkAnd [lhs, rhs]
                    Or -> mkOr [lhs, rhs]
                    Implication -> mkImplies lhs rhs
                    LessThan -> mkLt lhs rhs
                    LessThanEqual -> mkLe lhs rhs
                    GreaterThan -> mkGt lhs rhs
                    GreaterThanEqual -> mkGe lhs rhs
                    Equal -> mkEq lhs rhs
                    Minus -> mkSub [lhs, rhs]
                    Plus -> mkAdd [lhs, rhs]
                    Multiply -> mkMul [lhs, rhs]
                    Divide -> mkDiv lhs rhs
                    Alias -> undefined
            -- (NewStore e)          -> _
            (Forall var b)        -> do
                sym <- mkStringSymbol var
                z3body <- go ((var, sym) : env) b
                sort <- mkIntSort
                mkForall [] [sym] [sort] z3body
            (Exists var b)        -> do
                sym <- mkStringSymbol var
                z3body <- go ((var, sym) : env) b
                sort <- mkIntSort
                mkExists [] [sym] [sort] z3body
            -- (SizeOf var)          -> _
            -- (RepBy var i val)     -> _
            (Cond g e1 e2)        ->
                join $ liftM3 mkIte
                    (go env g)
                    (go env e1)
                    (go env e2)