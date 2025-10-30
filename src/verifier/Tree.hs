module Verifier.Tree where

import Control.Monad.Reader

import GCLParser.GCLDatatype
import Data.Maybe
import Control.Monad.State

type Typed = Type
type Untyped = ()
type Predicate = Expr

-- Environment for tree building
type Gamma = [(String, Type)]

data Tree
    = Empty
    | Next (Stmt Typed) Tree
    | Branch Bool (Expr Typed) Tree Tree
    | TWhile (Expr Typed) Tree Tree

instance Show Tree where
    show = prettyTree
    -- show = toGraphviz

pattern End :: Stmt Typed -> Tree
pattern End s = Next s Empty

instance Semigroup Tree where
    t <> Empty = t
    Empty <> t = t
    Next s1 t1 <> t2 = Next s1 (t1 <> t2)
    Branch b g t1 t2 <> t3 = Branch b g (t1 <> t3) (t2 <> t3)
    TWhile g b n <> t = TWhile g b (n <> t)

instance Monoid Tree where
    mempty = Empty

withVariables :: [(String, Type)] -> Reader Gamma a -> Reader Gamma a
withVariables g = local (g ++)

buildTree :: Stmt Untyped -> Reader Gamma Tree
buildTree = \case
    Seq s1 s2 -> liftM2 (<>) (buildTree s1) (buildTree s2)
    IfThenElse g t e -> Branch False <$> annotatePredicate g <*> buildTree t <*> buildTree e
    While g b -> TWhile <$> annotatePredicate g <*> buildTree b <*> pure Empty
    Block vars b -> withVariables [(x, t) | VarDeclaration x t <- vars] $ buildTree b
    Skip -> return $ End Skip
    Assert p -> End . Assert <$> annotatePredicate p
    Assume p -> End . Assume <$> annotatePredicate p
    Assign x e -> End . Assign x <$> annotatePredicate e
    AAssign x () i e -> do
        arrayTy <- asks $ fromJust . lookup x
        stmt <- AAssign x arrayTy <$> annotatePredicate i <*> annotatePredicate e
        return $ End stmt
    TryCatch e try catch -> do
        h <- withVariables [(e, PType PTInt)] $ buildTree catch
        try' <- buildTree try
        return $ insertRaises e h try'

insertRaises :: String -> Tree -> Tree -> Tree
insertRaises e h = \case
    Empty -> Empty
    Next (Assert p) t -> insertOnThrowingCondition p $
        Next (Assert p) (insertRaises e h t)
    Next (Assume p) t -> insertOnThrowingCondition p $
        Next (Assume p) (insertRaises e h t)
    Next (Assign x p) t -> insertOnThrowingCondition p $
        Next (Assign x p) (insertRaises e h t)
    Next (AAssign x ty i p) t -> insertOnThrowingCondition p $
        Branch True (BinopExpr LessThan i (SizeOf (Var x ty))) (insertRaises e h t) (Next (Assign e (LitI 2)) h)
    Branch True g l r -> Branch True g l r
    Branch False g l r -> insertOnThrowingCondition g $
        Branch False g (insertRaises e h l) (insertRaises e h r)
    TWhile g b t -> TWhile g (insertRaises e h b) (insertRaises e h t)
    _ -> error "Invalid tree"
    where
        insertOnThrowingCondition p t = case throwingCondition p of
            [] -> t
            cs -> foldr (\(code, cond) acc -> Branch True cond (Next (Assign e $ LitI code) h) acc) t cs

throwingCondition :: Expr Typed -> [(Int, Predicate Typed)]
throwingCondition = \case
    BinopExpr op x y -> throwingCondition x <> throwingCondition y <> case op of
        Divide -> [(1, BinopExpr Equal y (LitI 0))]
        _ -> []
    ArrayElem a i -> [(2, BinopExpr GreaterThanEqual i (SizeOf a))]
    Parens e -> throwingCondition e
    OpNeg e -> throwingCondition e
    Forall _ e -> throwingCondition e
    Exists _ e -> throwingCondition e
    RepBy a i e -> throwingCondition a <> throwingCondition i <> throwingCondition e
    Cond i t e -> throwingCondition i <> throwingCondition t <> throwingCondition e
    _ -> []

annotatePredicate :: Predicate Untyped -> Reader Gamma (Predicate Typed)
annotatePredicate = \case
    Var name _ -> asks (Var name . fromJust . lookup name)
    Parens inner -> Parens <$> annotatePredicate inner
    ArrayElem arr index -> ArrayElem <$> annotatePredicate arr <*> annotatePredicate index
    OpNeg rand -> OpNeg <$> annotatePredicate rand
    BinopExpr binop l r -> BinopExpr binop <$> annotatePredicate l <*> annotatePredicate r
    Forall name body -> Forall name <$> withVariables [(name, PType PTInt)] (annotatePredicate body)
    Exists name body -> Exists name <$> withVariables [(name, PType PTInt)] (annotatePredicate body)
    Cond cond then_ else_ -> liftM3 Cond
        (annotatePredicate cond)
        (annotatePredicate then_)
        (annotatePredicate else_)
    LitI n -> return $ LitI n
    LitB b -> return $ LitB b
    RepBy arr i v -> RepBy <$> annotatePredicate arr <*> annotatePredicate i <*> annotatePredicate v
    SizeOf arr -> SizeOf <$> annotatePredicate arr

prettyTree :: Tree -> String
prettyTree = go "" True
  where
    go pfx isLast = \case
        Empty           -> line pfx isLast ++ "∅"
        End stmt        -> line pfx isLast ++ show stmt
        Next stmt t     -> line pfx isLast ++ show stmt ++ "\n"
            ++ go (next pfx isLast) True t
        Branch _ expr l r -> line pfx isLast ++ "if " ++ show expr ++ "\n"
            ++ go (next pfx isLast) False l ++ "\n"
            ++ go (next pfx isLast) True r
        TWhile g b t -> line pfx isLast ++ "while " ++ show g ++ "\n"
            ++ go (next pfx isLast) False b ++ "\n"
            ++ go (next pfx isLast) True t

    line pfx isLast = pfx ++ if isLast then "└── " else "├── "
    next pfx isLast = pfx ++ if isLast then "    " else "│   "

toGraphviz :: Tree -> String
toGraphviz tree =
  "digraph Tree {\n" ++
  "  node [shape=box, style=rounded];\n" ++
  evalState (go tree) 0 ++ "}\n"
  where
    go :: Tree -> State Int String
    go t = do
        n <- state (\n -> (n, n + 1))
        case t of
            Empty           -> node "∅\", shape=\"plaintext\"" "gray" n
            End stmt        -> node (show stmt) "green" n
            Next stmt c     -> (\n1 e1 n2 -> n1 ++ e1 ++ n2) <$> node (show stmt) "orange" n <*> edge n <*> go c
            Branch _ g l r    -> (\n1 el nl er nr -> concat [n1, el, nl, er, nr])
                <$> node ("if "    ++ show g) "red" n <*> edge n <*> go l <*> edge n <*> go r
            TWhile g l r    -> (\n1 el nl er nr -> n1 ++ el ++ nl ++ er ++ nr)
                <$> node ("while " ++ show g) "red" n <*> edge n <*> go l <*> edge n <*> go r
    
    node :: String -> String -> Int -> State Int String
    node label color n = return $ "  n" ++ show n ++ " [label=\"" ++ label ++ "\", color=\"" ++ color ++ "\"];\n"
    
    edge :: Int -> State Int String
    edge f = gets (\t -> "  n" ++ show f ++ " -> n" ++ show t ++ ";\n")
