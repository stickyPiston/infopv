module Checker (buildTree, verify, runSE, SymbolicState(..), Stats(..)) where

import GCLParser.GCLDatatype

import System.Environment (getArgs)
import Z3.Monad as Z3 hiding (local)
import qualified Z3.Monad as Z3
import Control.Monad
import ExamplesOfSemanticFunction
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace

import qualified Data.Map as M

type Typed = Type
type Untyped = ()
type Predicate = Expr

data Stats = Stats
    { prunedPaths :: Int
    , inspectedPaths :: Int
    , pathsTooLong :: Int
    } deriving (Show)

instance Semigroup Stats where
    Stats p1 i1 l1 <> Stats p2 i2 l2 = Stats (p1 + p2) (i1 + i2) (l1 + l2)

instance Monoid Stats where
    mempty = Stats 0 0 0

-- Environment for tree building
type Gamma = [(String, Type)]

-- Environment for Expr to Z3 AST conversion
type SymbolEnv = [(String, Z3.Symbol)]

data Tree
    = Empty
    | Next (Stmt Typed) Tree
    | Branch (Expr Typed) Tree Tree
    deriving (Show)

pattern End :: Stmt Typed -> Tree
pattern End s = Next s Empty

instance Semigroup Tree where
    t <> Empty = t
    Empty <> t = t
    Next s1 t1 <> t2 = Next s1 (t1 <> t2)
    Branch g t1 t2 <> t3 = Branch g (t1 <> t3) (t2 <> t3)

instance Monoid Tree where
    mempty = Empty

buildTree :: Int -> Stmt Untyped -> Reader Gamma Tree
buildTree k = go
    where
        go :: Stmt Untyped -> Reader Gamma Tree
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
        -- TODO: Make sure variables in blocks get unique names
        unroll n g b = IfThenElse g (b `Seq` unroll (n - 1) g b) Skip

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

infixl 3 |->
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

fromExpr' :: Expr Typed -> SE Z3.AST
fromExpr' = lift . lift . fromExpr

data SymbolicState = SymbolicState
    { environment :: M.Map String (Expr Typed)
    , constraints :: Z3.AST
    , pathLength  :: Int
    }

-- | The Symbolic Eval (SE) monad
type SE = WriterT Stats (ReaderT SymbolicState Z3)

instance (Monoid s, MonadZ3 m) => MonadZ3 (WriterT s m) where
    getSolver = WriterT $ (, mempty) <$> getSolver
    getContext = WriterT $ (, mempty) <$> getContext

runSE :: SE a -> SymbolicState -> Z3 (a, Stats)
runSE = runReaderT . runWriterT

assume :: Expr Typed -> SE a -> SE a
assume p a = do
    z3Value <- fromExpr' p
    newConstraints <- asks constraints >>= mkAnd . (: [z3Value])
    local (\s -> s { constraints = newConstraints }) a

assign :: String -> Expr Typed -> SE a -> SE a
assign nm val = local \s -> s { environment = M.insert nm val $ environment s }

report :: Stats -> SE Bool
report s = tell s >> return True

pathTooLong, inspectedPath, prunedPath :: Stats
pathTooLong = mempty { pathsTooLong = 1 }
inspectedPath = mempty { inspectedPaths = 1 }
prunedPath = mempty { prunedPaths = 1 }

verify :: Tree -> SE Bool
verify t = asks pathLength >>= \case
    0 -> report pathTooLong
    _ -> case t of
        Empty -> report inspectedPath
        Next (Assume p) t -> do
            evaluatedCondition <- eval p
            assume evaluatedCondition $ continue t
        Next (Assert p) t -> eval p >>= checkValid >>= \case
            True -> continue t
            False -> trace ("Violated assertion: " ++ show p) $ return False
        Next (Assign nm val) t -> do
            val' <- eval val
            assign nm val' $ continue t
        Next (AAssign nm ann idx val) t -> do
            repby <- liftM3 RepBy (eval $ Var nm ann) (eval idx) (eval val)
            assign nm repby $ continue t
        Next Skip t -> continue t
        Branch g l r -> liftM2 (&&) (checkBranch g l) (checkBranch (OpNeg g) r)
    where
        continue :: Tree -> SE Bool
        continue t = local (\s -> s { pathLength = pathLength s - 1 }) $ verify t

        predicate :: Expr Typed -> SE Z3.AST
        predicate p = join (liftM2 mkImplies (asks constraints) (fromExpr' p))

        checkValid :: Expr Typed -> SE Bool
        checkValid p = Z3.local $ predicate p >>= mkNot >>= assert >> (Unsat ==) <$> check

        checkSatisfiability :: Expr Typed -> SE Z3.Result
        checkSatisfiability p = Z3.local $ predicate p >>= assert >> check

        checkBranch :: Expr Typed -> Tree -> SE Bool
        checkBranch g t = eval g >>= checkSatisfiability >>= \case
            Sat -> assume g $ continue t
            Unsat -> report prunedPath

        substStateVar :: String -> Expr Typed -> SE (Expr Typed)
        substStateVar fv e = asks \s -> (fv |-> environment s M.! fv) e

        eval :: Expr Typed -> SE (Expr Typed)
        eval e = foldM (flip substStateVar) e [fv | (fv, _) <- freeVariables e]