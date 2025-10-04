module Checker (buildTree, wlpTree, fromExpr) where

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

type Typed = Type
type Untyped = ()
type Predicate = Expr
type SymbolEnv = [(String, Z3.Symbol)]

type Gamma = [(String, Type)]

data Tree ann
    = Empty
    | Next (Stmt ann) (Tree ann)
    | Branch (Expr ann) (Tree ann) (Tree ann)
    deriving (Show)

pattern End :: Stmt a -> Tree a
pattern End s = Next s Empty

instance Semigroup (Tree ann) where
    t <> Empty = t
    Empty <> t = t
    Next s1 t1 <> t2 = Next s1 (t1 <> t2)
    Branch g t1 t2 <> t3 = Branch g (t1 <> t3) (t2 <> t3)

instance Monoid (Tree ann) where
    mempty = Empty

buildTree :: Int -> Stmt Untyped -> Reader Gamma (Tree Typed)
buildTree k = go
    where
        go :: Stmt Untyped -> Reader Gamma (Tree Typed)
        go = \case
            Seq s1 s2 -> liftM2 (<>) (go s1) (go s2)
            IfThenElse g t e -> Branch <$> annotatePredicate g <*> go t <*> go e
            While g b -> go $ unroll k g b
            Block vars b -> local ([(x, t) | VarDeclaration x t <- vars] ++) $ go b
            Skip -> return $ End Skip
            Assert p -> End . Assert <$> annotatePredicate p
            Assume p -> End . Assume <$> annotatePredicate p
            Assign x e -> End . Assign x <$> annotatePredicate e
            AAssign x () i e -> do
                arrayTy <- asks $ fromJust . lookup x
                stmt <- AAssign x arrayTy <$> annotatePredicate i <*> annotatePredicate e
                return $ End stmt
    
        unroll :: Int -> Expr Untyped -> Stmt Untyped -> Stmt Untyped
        unroll 0 g b = Assume $ OpNeg g
        unroll n g b = IfThenElse g (b `Seq` unroll (n - 1) g b) Skip

wlpTree :: Tree Typed -> Predicate Typed -> [Predicate Typed]
wlpTree Empty q = [q]
wlpTree (Next stmt child) q = map (wlp stmt) (wlpTree child q)
wlpTree (Branch g l r) q = map (g `opImplication`) (wlpTree l q) <> map (OpNeg g `opImplication`) (wlpTree r q)

wlp :: Stmt Typed -> Predicate Typed -> Predicate Typed
wlp stmt q = case stmt of
    Skip               -> q
    Assert p           -> p `opAnd` q
    Assume p           -> p `opImplication` q
    Assign var e       -> (var |-> e) q
    AAssign var a i e  -> (var |-> RepBy (Var var a) i e) q
    _ -> error "wlp should only be called on statements from the computation paths"

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