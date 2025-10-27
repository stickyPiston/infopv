module Verifier.Tree where

import GCLParser.GCLDatatype

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

type Typed = Type
type Untyped = ()
type Predicate = Expr

-- Environment for tree building
type Gamma = [(String, Type)]

data Tree
    = Empty
    | Next (Stmt Typed) Tree
    | Branch (Expr Typed) Tree Tree
    | TWhile (Expr Typed) Tree Tree

instance Show Tree where
    -- show = prettyTree
    show = toGraphviz

pattern End :: Stmt Typed -> Tree
pattern End s = Next s Empty

instance Semigroup Tree where
    t <> Empty = t
    Empty <> t = t
    Next s1 t1 <> t2 = Next s1 (t1 <> t2)
    Branch g t1 t2 <> t3 = Branch g (t1 <> t3) (t2 <> t3)
    TWhile g b n <> t = TWhile g b (n <> t)

instance Monoid Tree where
    mempty = Empty

buildTree :: Stmt Untyped -> Reader Gamma Tree
buildTree = \case
    Seq s1 s2 -> liftM2 (<>) (buildTree s1) (buildTree s2)
    IfThenElse g t e -> Branch <$> annotatePredicate g <*> buildTree t <*> buildTree e
    While g b -> TWhile <$> annotatePredicate g <*> buildTree b <*> pure Empty
    Block vars b -> local ([(x, t) | VarDeclaration x t <- vars] ++) $ buildTree b
    Skip -> return $ End Skip
    Assert p -> End . Assert <$> annotatePredicate p
    Assume p -> End . Assume <$> annotatePredicate p
    Assign x e -> End . Assign x <$> annotatePredicate e
    AAssign x () i e -> do
        arrayTy <- asks $ fromJust . lookup x
        stmt <- AAssign x arrayTy <$> annotatePredicate i <*> annotatePredicate e
        return $ End stmt

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

prettyTree :: Tree -> String
prettyTree = go "" True
  where
    go pfx isLast = \case
        Empty           -> line pfx isLast ++ "∅"
        End stmt        -> line pfx isLast ++ show stmt
        Next stmt t     -> line pfx isLast ++ show stmt ++ "\n"
            ++ go (next pfx isLast) True t
        Branch expr l r -> line pfx isLast ++ "if " ++ show expr ++ "\n"
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
            Branch g l r    -> (\n1 el nl er nr -> concat [n1, el, nl, er, nr])
                <$> node ("if "    ++ show g) "red" n <*> edge n <*> go l <*> edge n <*> go r
            TWhile g l r    -> (\n1 el nl er nr -> n1 ++ el ++ nl ++ er ++ nr)
                <$> node ("while " ++ show g) "red" n <*> edge n <*> go l <*> edge n <*> go r
    
    node :: String -> String -> Int -> State Int String
    node label color n = return $ "  n" ++ show n ++ " [label=\"" ++ label ++ "\", color=\"" ++ color ++ "\"];\n"
    
    edge :: Int -> State Int String
    edge f = gets (\t -> "  n" ++ show f ++ " -> n" ++ show t ++ ";\n")
