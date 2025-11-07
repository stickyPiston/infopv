module Main where

import Criterion.Main
import GCLParser.Parser
import Verifier.Checker
import Control.Monad

fromRight :: Either a b -> b
fromRight (Right b) = b

for :: [a] -> (a -> b) -> [b]
for = flip map

allOptions :: (Enum a, Bounded a) => [a]
allOptions = [minBound..maxBound]

main :: IO ()
main =
    completenessBench
    --heuristicBench

-- Benchmark all variants of a benchmark program with N in [2..10], assuming the files exist in the benchmark dir
completenessBench :: IO ()
completenessBench = do
    let bm = "invalidFind12"
        nums = map show [2..10]
        defaultConfig = VerifyConfig{n=50, ph=None, p=False, se=True}

    progs <- forM nums \file -> (file,) <$> fmap fromRight (parseGCLfile $ "examples/benchmark/" ++ bm ++ file ++ ".gcl")

    defaultMain [bgroup bm $ for progs \(file, p) -> bench file $ nfIO (verifyProgram defaultConfig p)]

-- Benchmark all 6 combinations of (no-)simplify and prune heuristic on multiple benchmarking progams
heuristicBench :: IO ()
heuristicBench = do
    let benchmarks = ["benchmark/invalidBsort"] 
        defaultConfig = VerifyConfig{n=40, ph=None, p=False, se=False}
        
    progs <- forM benchmarks \file -> (file,) <$> fmap fromRight (parseGCLfile $ "examples/" ++ file ++ ".gcl")

    defaultMain $
        for [(p, s) | p <- progs, s <- allOptions] \((file, p), se) -> bgroup (file ++ ", simplify: " ++ show se) $ 
            for allOptions \ph -> bench ("prune heuristic: " ++ show ph) $ 
                nfIO (verifyProgram defaultConfig{se=se, ph=ph} p)
