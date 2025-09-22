{-# LANGUAGE PatternSynonyms #-}

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
import Data.Foldable

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
type SymbolEnv = [(String, Z3.Symbol)]

type Gamma = [(String, Type)]

data Tree ann
    = Empty
    | Next (Stmt ann) (Tree ann)
    | Decl VarDeclaration (Tree ann)
    | Branch (Expr ann) (Tree ann) (Tree ann)

pattern End :: Stmt a -> Tree a
pattern End s = Next s Empty

instance Semigroup (Tree ann) where
    t <> Empty = t
    Empty <> t = t
    Next s1 t1 <> t2 = Next s1 (t1 <> t2)
    Decl decl t1 <> t2 = Decl decl (t1 <> t2)
    Branch g t1 t2 <> t3 = Branch g (t1 <> t3) (t2 <> t3)

instance Monoid (Tree ann) where
    mempty = Empty

buildTree :: Int -> Stmt Untyped -> Reader Gamma (Tree Typed)
buildTree k = go k
    where
        go :: Int -> Stmt Untyped -> Reader Gamma (Tree Typed)
        go n = \case
            Seq s1 s2 -> buildTree k s1 <> buildTree k s2
            IfThenElse g t e -> Branch <$> annotatePredicate g <*> buildTree k t <*> buildTree k e
            While g b -> unroll k g b
            Block vars b -> local ([(x, t) | VarDeclaration x t <- vars] ++) $ buildTree k b
            Skip -> return $ End Skip
            Assert p -> End . Assert <$> annotatePredicate p
            Assume p -> End . Assume <$> annotatePredicate p
            Assign x e -> End . Assign x <$> annotatePredicate e
            AAssign x i e -> (End .) . AAssign x <$> annotatePredicate i <*> annotatePredicate e
    
        unroll :: Int -> Expr Untyped -> Stmt Untyped -> Reader Gamma (Tree Typed)
        unroll 0 _ b = go k b
        unroll n g b = go (n - 1) (IfThenElse g (While g b) Skip)

wlp :: Tree Typed -> Predicate Typed -> Predicate Typed
wlp stmt q = case stmt of
    Skip               -> return q
    Assert p           -> (`opAnd` q) <$> annotatePredicate p
    Assume p           -> (`opImplication` q) <$> annotatePredicate p
    Assign var e       -> do
        ae <- annotatePredicate e
        return $ (var |-> ae) q
    AAssign var i e    -> do
        repby <- liftM3 RepBy
            (annotatePredicate (Var var ()))
            (annotatePredicate i)
            (annotatePredicate e)
        return $ (var |-> repby) q
    -- Seq s1 s2          -> wlp s1 =<< wlp s2 q
    -- IfThenElse g s1 s2 -> do
    --     l <- liftM2 opImplication (annotatePredicate g) (wlp s1 q)
    --     r <- liftM2 opImplication (OpNeg <$> annotatePredicate g) (wlp s2 q)
    --     return $ l `opAnd` r
    -- While guard s      -> undefined
    -- Block vars s       -> local ([(name, ty) | VarDeclaration name ty <- vars] ++) do
    --     -- base <- wlp s q
    --     -- return $ foldr Forall base [name | VarDeclaration name _ <- vars]
    --     wlp s q

annotatePredicate :: Predicate Untyped -> Reader Gamma (Predicate Typed)
annotatePredicate = \case
    Var name _ -> asks (Var name . fromJust . lookup name)
    Parens inner -> Parens <$> annotatePredicate inner
    ArrayElem arr index -> ArrayElem <$> annotatePredicate arr <*> annotatePredicate index
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
    RepBy arr i v -> RepBy <$> annotatePredicate arr <*> annotatePredicate i <*> annotatePredicate v
    SizeOf arr -> SizeOf <$> annotatePredicate arr

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
    SizeOf a -> SizeOf $ (x |-> for) a
    RepBy a i v -> RepBy
        ((x |-> for) a)
        ((x |-> for) i)
        ((x |-> for) v)
    ArrayElem a i -> ArrayElem
        ((x |-> for) a)
        ((x |-> for) i)
    _ -> in_

fromType :: Type -> Z3 Z3.Sort
fromType = \case
    PType PTInt -> mkIntSort
    PType PTBool -> mkBoolSort
    RefType -> undefined
    AType ty -> join $ mkArraySort <$> mkIntSort <*> fromType (PType ty)

fromExpr :: Expr Type -> Z3 Z3.AST
fromExpr e = do
    env <- foldM buildEnv ([], []) (freeVariables e)
    go env e
    where
        buildEnv :: (SymbolEnv, SymbolEnv) -> (String, Type) -> Z3 (SymbolEnv, SymbolEnv)
        buildEnv (varEnv, arrEnv) (name, ty) = do
            left <- (name,) <$> mkStringSymbol name
            (left : varEnv,) <$> case ty of
                AType _ -> (: arrEnv) . (name,) <$> mkStringSymbol ('#' : name)
                _ -> return arrEnv
        
        -- Take tuple of (variable environment, array size environment) and an expression,
        -- and produce the corresponding Z3 AST.
        go :: (SymbolEnv, SymbolEnv) -> Expr Type -> Z3 Z3.AST
        go env@(varEnv, arrEnv) = \case
            (Var var ty)             -> do
                sort <- fromType ty
                mkVar (fromJust $ lookup var varEnv) sort
            (LitI x)              -> mkInt x =<< mkIntSort
            (LitB b)           -> mkBool b
            -- LitNull               -> _
            -- (Dereference u)       -> _
            (Parens e)            -> go env e
            (ArrayElem var index) -> join $ mkSelect <$> go env var <*> go env index
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
                z3body <- go ((var, sym) : varEnv, arrEnv) b
                sort <- mkIntSort
                mkForall [] [sym] [sort] z3body
            (Exists var b)        -> do
                sym <- mkStringSymbol var
                z3body <- go ((var, sym) : varEnv, arrEnv) b
                sort <- mkIntSort
                mkExists [] [sym] [sort] z3body
            (SizeOf (Var name t)) -> mkVar (fromJust $ lookup name arrEnv) =<< mkIntSort
            (RepBy var i val)     -> join $ mkStore <$> go env var <*> go env i  <*> go env val
            (Cond g e1 e2)        -> join $ mkIte   <$> go env g   <*> go env e1 <*> go env e2