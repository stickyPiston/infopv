{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module GCLParser.GCLDatatype where
    
-- import Data.List

type Verbose = Bool

type Depth = Int

-----------------------------------------------------------------------------
-- Program
-----------------------------------------------------------------------------

data PrimitiveType 
    = PTInt 
    | PTBool
    deriving (Show, Eq)

data Type 
    = PType PrimitiveType  -- primitive tyoe
    | RefType
    | AType PrimitiveType  -- array type, one dimensional
    deriving (Show, Eq)

data VarDeclaration 
    = VarDeclaration String Type
    deriving (Show)

{-
data Procedure 
    = Procedure String [VarDeclaration] [VarDeclaration] Expr Expr
    deriving (Show)
-}

data Program ann
    = Program { 
--                pre    :: Expr, 
              name   :: String 
              , input  :: [VarDeclaration]
              , output :: [VarDeclaration]
              , stmt   :: Stmt ann
--              , procs  :: [Procedure]
--              , post   :: Expr 
              } 
    deriving (Show)

data Stmt ann
    = Skip       
    | Assert     (Expr ann)             
    | Assume     (Expr ann)             
    | Assign     String           (Expr ann)   
    -- | AAssign    String           Expr   Expr  
    -- | DrefAssign String           Expr
    | Seq        (Stmt ann)             (Stmt ann)   
    | IfThenElse (Expr ann)             (Stmt ann)   (Stmt ann)     
    | While      (Expr ann)             (Stmt ann)   
    | Block      [VarDeclaration] (Stmt ann)   
    -- | TryCatch   String           Stmt   Stmt
--    | Call       [String]         [Expr] String

instance Show (Stmt ann) where
    show Skip                     = "skip"
    show (Assert condition)       = "assert " ++ show condition
    show (Assume condition)       = "assume " ++ show condition
    show (Assign var e)           = var ++ " := " ++ show e 
    -- show (DrefAssign var e)       = var ++ ".val := " ++ show e 
    -- show (AAssign var i e)        = var ++ "[" ++ show i ++ "]" ++ " := " ++ show e
    show (Seq s1 s2)              = show s1 ++ ";" ++ show s2 
    show (IfThenElse gaurd s1 s2) = "if " ++ show gaurd ++ " then " ++ show s1 ++ " else " ++ show s2
    show (While gaurd s)          = "while " ++ show gaurd ++ " do {" ++ show s ++ "}"
    show (Block vars s)           = "var " ++ show vars ++ " {" ++ show s ++ "}"
    -- show (TryCatch e s1 s2)         = "try { " ++ show s1 ++ " } catch(" ++ e ++ "){ " ++ show s2 ++ " }"
--    show (Call vars args f)       = "(" ++ intercalate "," vars ++ ") := " ++ "(" ++ (intercalate "," . map show) args ++ ")"
    
-----------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------
    
data Expr ann
    = Var                String ann
    | LitI               Int     
    | LitB               Bool    
    -- | LitNull
    | Parens             (Expr ann)    
    -- | ArrayElem          Expr   Expr   
    | OpNeg              (Expr ann)    
    | BinopExpr          BinOp  (Expr ann)   (Expr ann)
    | Forall             String (Expr ann) 
    | Exists             String (Expr ann) 
    -- | SizeOf             Expr
    -- | RepBy              Expr   Expr   Expr
    | Cond               (Expr ann)   (Expr ann)   (Expr ann)
    -- | NewStore           Expr
    -- | Dereference        String
    deriving (Eq) 

data BinOp = And | Or | Implication 
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Equal
    | Minus | Plus | Multiply | Divide
    | Alias
    deriving (Eq)

opAnd :: Expr a -> Expr a -> Expr a
opAnd = BinopExpr And
opOr :: Expr a -> Expr a -> Expr a
opOr  = BinopExpr Or
opImplication :: Expr a -> Expr a -> Expr a
opImplication = BinopExpr Implication
opLessThan :: Expr a -> Expr a -> Expr a
opLessThan = BinopExpr LessThan
opLessThanEqual :: Expr a -> Expr a -> Expr a
opLessThanEqual = BinopExpr LessThanEqual
opGreaterThan :: Expr a -> Expr a -> Expr a
opGreaterThan   = BinopExpr GreaterThan
opGreaterThanEqual :: Expr a -> Expr a -> Expr a
opGreaterThanEqual = BinopExpr GreaterThanEqual
opEqual :: Expr a -> Expr a -> Expr a
opEqual = BinopExpr Equal
opMinus :: Expr a -> Expr a -> Expr a
opMinus = BinopExpr Minus
opPlus :: Expr a -> Expr a -> Expr a
opPlus = BinopExpr Plus
opMultiply :: Expr a -> Expr a -> Expr a
opMultiply = BinopExpr Multiply
opDivide :: Expr a -> Expr a -> Expr a
opDivide = BinopExpr Divide
opAlias :: Expr a -> Expr a -> Expr a
opAlias    = BinopExpr Alias
    
instance Show (Expr ann) where
    show (Var var _)                = var
    show (LitI x)                   = show x
    show (LitB True)                = "true"
    show (LitB False)               = "false"
    -- show LitNull                    = "null"
    -- show (Dereference u)            = u ++ ".val"
    show (Parens e)                 = "(" ++ show e ++ ")"
    -- show (ArrayElem var index)      = show var ++ "[" ++ show index ++ "]"
    show (OpNeg expr)               = "~" ++ show expr
    show (BinopExpr op e1 e2)       = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    -- show (NewStore e)               = "new(" ++ show e ++ ")"    
    show (Forall var p)             = "forall " ++ var ++ ":: " ++ show p
    show (Exists var p)             = "exists " ++ var ++ ":: " ++ show p
    -- show (SizeOf var)               = "#" ++ show var
    -- show (RepBy var i val)          = show var ++ "(" ++ show i ++ " repby " ++ show val ++ ")"
    show (Cond g e1 e2)             = "(" ++ show g ++ " -> " ++ show e1 ++ " | " ++ show e2 ++ ")"
    
instance Show BinOp where
    show And = "&&"
    show Or = "||" 
    show Implication = "==>" 
    show LessThan = "<"
    show LessThanEqual = "<=" 
    show GreaterThan = ">"
    show GreaterThanEqual = ">="
    show Equal = "="
    show Minus = "-" 
    show Plus = "+"
    show Multiply = "*"
    show Divide = "/"
    show Alias = "=="  
