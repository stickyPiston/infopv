module Main where

import Criterion.Main
import GCLParser.Parser
import Verifier.Checker

fromRight :: Either a b -> b
fromRight (Right b) = b

main :: IO ()
main = do
    [min, e, prune] <- mapM (fmap fromRight . parseGCLfile) ["examples/" ++ file ++ ".gcl" | file <- ["min", "E", "pruning"]]
    defaultMain 
        [ bgroup "min.gcl"     [ bench "k=10 n=20 None"   $ nfIO (verifyProgram 10 20 None min)
                               , bench "k=10 n=20 Length" $ nfIO (verifyProgram 10 20 LengthBased min)
                               , bench "k=10 n=20 Full"   $ nfIO (verifyProgram 10 20 Full min)
                               ]
        , bgroup "E.gcl"       [ bench "k=10 n=20 None"   $ nfIO (verifyProgram 10 20 None e)
                               , bench "k=10 n=20 Length" $ nfIO (verifyProgram 10 20 LengthBased e)
                               , bench "k=10 n=20 Full"   $ nfIO (verifyProgram 10 20 Full e)
                               ]
        , bgroup "pruning.gcl" [ bench "k=10 n=20 None"   $ nfIO (verifyProgram 10 20 None prune)
                               , bench "k=10 n=20 Length" $ nfIO (verifyProgram 10 20 LengthBased prune)
                               , bench "k=10 n=20 Full"   $ nfIO (verifyProgram 10 20 Full prune)
                               ]
        ]
    