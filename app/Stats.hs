module Stats where

data Stats = Stats
    { inspectedPaths :: Int
    , prunedPaths :: Int
    }

instance Show Stats where
    show (Stats i p) = "Statistics:\n\tInspected paths: " ++ show i ++ "\n\tPruned paths:    " ++ show p

instance Semigroup Stats where
    Stats i1 p1 <> Stats i2 p2 = Stats (i1 + i2) (p1 + p2)

instance Monoid Stats where
    mempty = Stats 0 0
