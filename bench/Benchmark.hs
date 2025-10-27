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
main = do
    let benchmarks = ["pruning"] 
        defaultConfig = VerifyConfig 20 None False False
        
    progs <- forM benchmarks \file -> (file,) <$> fmap fromRight (parseGCLfile $ "examples/" ++ file ++ ".gcl")

    -- All combinations of (no-)simplify and prune heuristic
    defaultMain $
        for [(p, s) | p <- progs, s <- allOptions] \((file, p), se) -> bgroup (file ++ ", simplify: " ++ show se) $ 
            for allOptions \ph -> bench ("prune heuristic: " ++ show ph) $ 
                nfIO (verifyProgram defaultConfig{se=se, ph=ph} p)