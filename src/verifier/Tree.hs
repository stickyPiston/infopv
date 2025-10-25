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
    | Branch (Expr Typed) Tree Tree

instance Show Tree where
    --show = prettyTree
    show = toGraphviz

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
        unroll 0 g _ = Assume $ OpNeg g
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

    line pfx isLast = pfx ++ (if isLast then "└── " else "├── ")
    next pfx isLast = pfx ++ (if isLast then "    " else "│   ")

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
            Next stmt c     -> gets ((\e1 n1 n2 -> n1 ++ e1 ++ n2) . edge n) <*> node (show stmt) "orange" n <*> go c
            Branch expr l r -> 
                gets ((\el n1 nl er nr -> n1 ++ el ++ nl ++ er ++ nr) . edge n) <*> node ("if " ++ show expr) "red" n <*> go l <*> gets (edge n) <*> go r
    
    node :: String -> String -> Int -> State Int String
    node label color n = return $ "  n" ++ show n ++ " [label=\"" ++ label ++ "\", color=\"" ++ color ++ "\"];\n"
    
    edge :: Int -> Int -> String
    edge f t = "  n" ++ show f ++ " -> n" ++ show t ++ ";\n"
