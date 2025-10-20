module Stats where

data Stats = Stats
    { inspectedPaths :: Int
    , prunedPaths :: Int
    , pathsTooLong :: Int
    }

instance Show Stats where
    show (Stats i p l) = unlines
        [ "Statistics:"
        , "\tTotal inspected paths: " ++ show (i + p + l)
        , "\tFull paths:            " ++ show i
        , "\tPruned paths:          " ++ show p
        , "\tPaths too long:        " ++ show l
        ]

instance Semigroup Stats where
    Stats i1 p1 l1 <> Stats i2 p2 l2 = Stats (i1 + i2) (p1 + p2) (l1 + l2)

instance Monoid Stats where
    mempty = Stats 0 0 0

pathTooLong, inspectedPath, prunedPath :: Stats
inspectedPath = mempty { inspectedPaths = 1 }
pathTooLong = mempty { pathsTooLong = 1 }
prunedPath = mempty { prunedPaths = 1 }
