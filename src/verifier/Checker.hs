{-# OPTIONS_GHC -Wno-orphans #-}
module Verifier.Checker where

import GCLParser.GCLDatatype

import Z3.Monad as Z3 hiding (local, simplify)
import qualified Z3.Monad as Z3
import Control.Monad
import ExamplesOfSemanticFunction
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import System.Random.Stateful

import qualified Data.Map as M

import Verifier.Tree
import Verifier.Stats
import Verifier.Simplifier
import Debug.Trace

-- Environment for Expr to Z3 AST conversion
type SymbolEnv = [(String, Z3.Symbol)]

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
            (LitB b)              -> mkBool b
            -- LitNull               -> _
            -- (Dereference u)       -> _
            (Parens e')           -> go env e'
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
                    Alias -> mkEq lhs rhs
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
            (SizeOf (Var name _)) -> mkVar (fromJust $ lookup name arrEnv) =<< mkIntSort
            (RepBy var i val)     -> join $ mkStore <$> go env var <*> go env i  <*> go env val
            (Cond g e1 e2)        -> join $ mkIte   <$> go env g   <*> go env e1 <*> go env e2
            _ -> error "Invalid expression type"

fromExpr' :: Expr Typed -> SE Z3.AST
fromExpr' = lift . fromExpr

data PruneHeuristic
    = None
    | Full
    | LengthBased
    deriving (Show, Read)

data SymbolicState = SymbolicState
    { environment    :: M.Map String (Expr Typed)
    , constraints    :: [Expr Typed]
    , pathLength     :: Int
    , maxLength      :: Int
    , pruneHeuristic :: PruneHeuristic
    }

-- | The Symbolic Eval (SE) monad
type SE = RWST SymbolicState Stats StdGen Z3

instance (Monoid w, MonadZ3 m) => MonadZ3 (RWST r w s m) where
    getSolver = RWST \ _ s -> (, s, mempty) <$> getSolver
    getContext = RWST \ _ s -> (, s, mempty) <$> getContext

runSE :: SE a -> SymbolicState -> StdGen -> Z3 (a, Stats)
runSE = evalRWST

assume :: Expr Typed -> SE a -> SE a
assume (LitB _) = id
assume p = local (\s -> s { constraints = simplify p : constraints s })

randomRange :: Int -> Int -> SE Int
randomRange lo hi = do
    g <- get
    let (a, g') = randomR (lo, hi) g
    modify $ const g'
    return a

assign :: String -> Expr Typed -> SE a -> SE a
assign nm val = local \s -> s { environment = M.insert nm val $ environment s }

report :: Stats -> SE Bool
report s = tell s >> return True

verify :: Tree -> SE Bool
verify tree = asks pathLength >>= \case
    0 -> report pathTooLong
    _ -> case tree of
        Empty -> report inspectedPath
        Next (Assume p) t -> do
            evaluatedCondition <- evalSE p
            assume evaluatedCondition $ continue t
        Next (Assert p) t -> do
            simpleP <- simplify <$> evalSE p
            traceM $ "Checking validity of " <> show simpleP
            checkValid simpleP >>= \case
                True -> continue t
                False -> tell (violatedAssertion simpleP) >> return False
        Next (Assign nm val) t -> do
            val' <- evalSE val
            assign nm val' $ continue t
        Next (AAssign nm ann idx val) t -> do
            repby <- liftM3 RepBy (evalSE $ Var nm ann) (evalSE idx) (evalSE val)
            assign nm repby $ continue t
        Next Skip t -> continue t
        Branch g l r -> liftM2 (&&) (checkBranch g l) (checkBranch (OpNeg g) r)
        _ -> error "Invalid statement in tree"
    where
        continue :: Tree -> SE Bool
        continue t = local (\s -> s { pathLength = pathLength s - 1 }) $ verify t

        checkValid :: Expr Typed -> SE Bool
        checkValid (LitB x) = return x
        checkValid p = Z3.local do
            relevantConstraints <- asks (filterIrrelevantConstraints p . constraints) >>= mapM fromExpr' >>= mkAnd
            z3Predicate <- fromExpr' p
            (relevantConstraints `mkImplies` z3Predicate) >>= mkNot >>= assert >> (Unsat ==) <$> check

        prune :: Expr Typed -> SE Z3.Result
        prune p = Z3.local do
            relevantConstraints <- asks (filterIrrelevantConstraints p . constraints) >>= mapM fromExpr'
            fromExpr' p >>= mkAnd . (: relevantConstraints) >>= assert >> check

        checkSatisfiability :: Expr Typed -> SE Z3.Result
        checkSatisfiability (LitB x) = return if x then Sat else Unsat
        checkSatisfiability p = asks pruneHeuristic >>= \case
            Full -> prune p
            None -> return Sat
            LengthBased -> do
                r <- asks maxLength >>= randomRange 0
                l <- asks pathLength
                if r < l then prune p else return Sat

        checkBranch :: Expr Typed -> Tree -> SE Bool
        checkBranch g t = do
            evalG <- evalSE g
            checkSatisfiability evalG >>= \case
                Sat -> assume evalG $ continue t
                Unsat -> report prunedPath
                Undef -> error "Undefined SE Result"

        substStateVar :: String -> Expr Typed -> SE (Expr Typed)
        substStateVar fv e = asks \s -> (fv |-> environment s M.! fv) e

        evalSE :: Expr Typed -> SE (Expr Typed)
        evalSE e = foldM (flip substStateVar) e [fv | (fv, _) <- freeVariables e]

verifyProgram :: Int -> Int -> PruneHeuristic -> Bool -> Program -> IO (Bool, Stats)
verifyProgram k n ph p Program{stmt, input, output} = do 
    let initialGamma = [(name, ty) | VarDeclaration name ty <- input ++ output]
    let compTree = runReader (buildTree k stmt) initialGamma
    when p $ print compTree
    let initialRho = M.fromList [(name, Var (name ++ "_0") ty) | VarDeclaration name ty <- input ++ output]
    g <- initStdGen
    evalZ3 do
        let initialSymbolicState = SymbolicState { environment = initialRho, pathLength = n, constraints = [], maxLength = k, pruneHeuristic = ph }
        runSE (verify compTree) initialSymbolicState g
