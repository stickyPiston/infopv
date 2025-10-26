module Verifier.Stats where

import GCLParser.GCLDatatype ( Type, Expr, size )

import qualified Data.Set as S
import Control.DeepSeq (NFData)
import GHC.Generics

data Stats = Stats
    { inspectedPaths :: Int
    , prunedPaths :: Int
    , pathsTooLong :: Int
    , violatedAssertions :: S.Set (Expr Type)
    , checkedFormulaSize :: Int
    , z3Invocations :: Int
    }
    deriving (Generic, NFData)

instance Show Stats where
    show (Stats i p l va f z) = unlines $
        [ "Statistics:"
        , "\tTotal inspected paths:      " ++ show (i + p + l)
        , "\tFull paths:                 " ++ show i
        , "\tPruned paths:               " ++ show p
        , "\tPaths too long:             " ++ show l
        , "\tTotal checked formula size: " ++ show f
        , "\tZ3 invocations:             " ++ show z
        ]
        ++ if not (null va)
            then "Violated assertions:" : map show (S.toList va)
            else []

instance Semigroup Stats where
    Stats i1 p1 l1 va1 f1 z1 <> Stats i2 p2 l2 va2 f2 z2
        = Stats (i1 + i2) (p1 + p2) (l1 + l2) (va1 <> va2) (f1 + f2) (z1 + z2)

instance Monoid Stats where
    mempty = Stats 0 0 0 mempty 0 0

pathTooLong, inspectedPath, prunedPath, z3Invocation :: Stats
inspectedPath = mempty { inspectedPaths = 1 }
pathTooLong = mempty { pathsTooLong = 1 }
prunedPath = mempty { prunedPaths = 1 }
z3Invocation = mempty { z3Invocations = 1 }

violatedAssertion :: Expr Type -> Stats
violatedAssertion p = mempty { violatedAssertions = S.singleton p }

checkedFormula :: Expr Type -> Stats
checkedFormula e = mempty { checkedFormulaSize = size e }