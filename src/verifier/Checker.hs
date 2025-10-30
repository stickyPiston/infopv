{-# OPTIONS_GHC -Wno-orphans #-}
module Verifier.Checker where

import GCLParser.GCLDatatype
import ExamplesOfSemanticFunction

import Verifier.Tree
import Verifier.Simplifier
import Verifier.Stats

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Maybe
import Data.Functor
import System.Random
import Debug.Trace

import Z3.Monad as Z3 hiding (local, simplify, eval)
import qualified Z3.Monad as Z3
import qualified Data.Map as M
import Data.List (isPrefixOf)

-- Environment for Expr to Z3 AST conversion
type SymbolEnv = [(String, Z3.AST)]

repbySpineRoot :: Expr a -> Maybe (Expr a)
repbySpineRoot (Var x a) = Just (Var x a)
repbySpineRoot (RepBy a _ _) = repbySpineRoot a
repbySpineRoot _ = Nothing

substRepbySpineRoot :: String -> Expr a -> Expr a -> Maybe (Expr a)
substRepbySpineRoot name for (Var x _)
    | x == name = Just for
    | otherwise = Nothing
substRepbySpineRoot name for (RepBy a _ _) = substRepbySpineRoot name for a 
substRepbySpineRoot _ _ _ = Nothing

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
    SizeOf a
        | "#" `isPrefixOf` x -> fromMaybe a $ substRepbySpineRoot (tail x) for a
        | otherwise -> SizeOf $ (x |-> for) a
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
            sort <- fromType ty
            left <- (name,) <$> (mkStringSymbol name >>= flip mkVar sort)
            (left : varEnv,) <$> case ty of
                AType _ -> do
                    intSort <- mkIntSort
                    (: arrEnv) . (name,) <$> (mkStringSymbol ('#' : name) >>= flip mkVar intSort)
                _ -> return arrEnv

        -- Take tuple of (variable environment, array size environment) and an expression,
        -- and produce the corresponding Z3 AST.
        go :: (SymbolEnv, SymbolEnv) -> Expr Type -> Z3 Z3.AST
        go env@(varEnv, arrEnv) = \case
            (Var var _)          -> return $ fromJust $ lookup var varEnv
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
                sort <- mkIntSort
                arg <- mkBound 0 sort
                z3body <- go ((var, arg) : varEnv, arrEnv) b
                mkForall [] [sym] [sort] z3body
            (Exists var b)        -> do
                sym <- mkStringSymbol var
                sort <- mkIntSort
                arg <- mkBound 0 sort
                z3body <- go ((var, arg) : varEnv, arrEnv) b
                mkExists [] [sym] [sort] z3body
            (SizeOf (RepBy name _ _)) -> go env (SizeOf name)
            (SizeOf (Var name _)) -> return (fromJust $ lookup name arrEnv)
            (RepBy var i val)     -> join $ mkStore <$> go env var <*> go env i  <*> go env val
            (Cond g e1 e2)        -> join $ mkIte   <$> go env g   <*> go env e1 <*> go env e2
            e' -> error $ "Invalid expression type: " <> show e'

fromExpr' :: Expr Typed -> SE Z3.AST
fromExpr' = lift . fromExpr

data PruneHeuristic
    = None
    | Full
    | LengthBased
    deriving (Show, Read, Enum, Bounded)

data SymbolicState = SymbolicState
    { environment     :: M.Map String (Expr Typed)
    , equalities      :: M.Map String (Expr Typed)
    , constraints     :: [Expr Typed]
    , pathLength      :: Int
    , maxLength       :: Int
    , pruneHeuristic  :: PruneHeuristic
    , simplifyEnabled :: Bool
    }

-- | The Symbolic Eval (SE) monad
type SE = RWST SymbolicState Stats StdGen Z3

instance (Monoid w, MonadZ3 m) => MonadZ3 (RWST r w s m) where
    getSolver = RWST \ _ s -> (, s, mempty) <$> getSolver
    getContext = RWST \ _ s -> (, s, mempty) <$> getContext

runSE :: SE a -> SymbolicState -> StdGen -> Z3 (a, Stats)
runSE = evalRWST

isConcrete :: Expr Typed -> Bool
isConcrete (LitI _) = True
isConcrete (LitB _) = True
isConcrete _ = False

assume :: Expr Typed -> SE a -> SE a
assume = \case
    LitB _ -> id

    BinopExpr Equal (Var x _) b | isConcrete b ->
        local \s -> s { equalities = M.insert x b $ equalities s }

    p@(BinopExpr Equal (SizeOf a) b) | isConcrete b -> case repbySpineRoot a of
        Just (Var x _) -> local \s -> s { equalities = M.insert ('#' : x) b $ equalities s }
        _ -> local (\s -> s { constraints = p : constraints s })

    BinopExpr Equal a b | isConcrete a -> assume (BinopExpr Equal b a)

    p -> local \s -> s { constraints = p : constraints s }

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

failWith :: Stats -> SE Bool
failWith s = tell s >> return False

checkValid :: Expr Typed -> SE Bool
checkValid (LitB x) = tell (checkedFormula (LitB x)) $> x
checkValid p = Z3.local do
    cs <- asks constraints >>= mapM fromExpr' >>= mkAnd
    z3Predicate <- fromExpr' p
    tell $ checkedFormula p <> z3Invocation
    (cs `mkImplies` z3Predicate) >>= mkNot >>= assert >> (Unsat ==) <$> check

prune :: Expr Typed -> SE Z3.Result
prune p = Z3.local do
    cs <- asks constraints >>= mapM fromExpr'
    tell $ checkedFormula p <> z3Invocation
    fromExpr' p >>= mkAnd . (: cs) >>= assert >> check

checkSatisfiability :: Expr Typed -> SE Z3.Result
checkSatisfiability (LitB x) = do
    tell (checkedFormula $ LitB x)
    return if x then Sat else Unsat
checkSatisfiability p = asks pruneHeuristic >>= \case
    Full -> prune p
    None -> return Sat
    LengthBased -> do
        r <- asks maxLength >>= randomRange 0
        l <- asks pathLength
        if r < l then prune p else return Sat

checkBranch :: Expr Typed -> Tree -> SE Bool
checkBranch g t = do
    evalG <- eval g
    checkSatisfiability evalG >>= \case
        Sat -> assume evalG $ continue t
        Unsat -> report prunedPath
        Undef -> error "Undefined SE Result"

eval :: Expr Typed -> SE (Expr Typed)
eval e = asks \s ->
    let substExpr = foldl (\acc fv -> fv |-> environment s M.! fv $ acc) e (map fst $ freeVariables e)
        equalExpr = foldl (\acc (x, for) -> x |-> for $ acc) substExpr (M.toList $ equalities s)
     in if simplifyEnabled s then simplify equalExpr else equalExpr

continue :: Tree -> SE Bool
continue t = local (\s -> s { pathLength = pathLength s - 1 }) $ verify t

verify :: Tree -> SE Bool
verify tree = asks pathLength >>= \case
    0 -> report pathTooLong
    _ -> case tree of
        Empty -> report inspectedPath
        Next (Assume p) t -> do
            evaluatedCondition <- eval p
            assume evaluatedCondition $ continue t
        Next (Assert p) t -> do
            evaluatedCondition <- eval p
            checkValid evaluatedCondition >>= \case
                True -> continue t
                False -> failWith $ violatedAssertion p
        Next (Assign nm val) t -> do
            val' <- eval val
            assign nm val' $ continue t
        Next (AAssign nm ann idx val) t -> do
            repby <- liftM3 RepBy (eval $ Var nm ann) (eval idx) (eval val)
            assign nm repby $ continue t
        Next Skip t -> continue t
        Branch _ g l r -> liftM2 (&&) (checkBranch g l) (checkBranch (OpNeg g) r)
        TWhile g b t -> verify $ Branch False g (b <> TWhile g b t) t
        _ -> error "Invalid tree node"

data VerifyConfig = VerifyConfig
    { n    :: Int
    , ph   :: PruneHeuristic
    , p    :: Bool
    , se   :: Bool
    }

verifyProgram :: VerifyConfig -> Program -> IO (Bool, Stats)
verifyProgram VerifyConfig{n, ph, p, se} Program{stmt, input, output} = do 
    let initialGamma = [(name, ty) | VarDeclaration name ty <- input ++ output]
    let compTree = runReader (buildTree stmt) initialGamma
    when p $ print compTree
    let initialRho = M.fromList [(name, Var (name ++ "_0") ty) | VarDeclaration name ty <- input ++ output]
    g <- initStdGen
    evalZ3
        let initialSymbolicState = SymbolicState
                { environment = initialRho
                , equalities = mempty
                , pathLength = n
                , constraints = []
                , maxLength = n
                , pruneHeuristic = ph
                , simplifyEnabled = se
                }
         in runSE (verify compTree) initialSymbolicState g
