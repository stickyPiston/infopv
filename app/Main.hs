module Main where

import GCLUtils
import GCLParser.GCLDatatype

import System.Environment (getArgs)
import Z3.Monad as Z3 hiding (local)
import Control.Monad
import Control.Monad.Reader
import Options.Applicative
import Debug.Trace
import qualified Data.Map as M

import Checker

data Config = Config
    { k :: Int
    , filepath :: String
    }

configParser :: ParserInfo Config
configParser = info parser fullDesc
    where
        parser = Config
            <$> option auto
                ( long "k"
                <> help "The number of times a while loop is unrolled"
                <> showDefault
                <> value 10
                <> metavar "INT"
                )
            <*> argument str (metavar "FILE")

main :: IO ()
main = do
    Config { k, filepath } <- execParser configParser
    parseGCLfile filepath >>= \case
        Left err -> putStrLn $ "Could not parse " <> filepath <> ": " <> err
        Right Program { stmt, input, output } -> do
            let initialGamma = [(name, ty) | VarDeclaration name ty <- input ++ output]
            let compTree = runReader (buildTree k stmt) initialGamma
            (prunedCompTree, stats) <- runPrune (prune [] compTree) (M.fromList [(name, Var name ty) | VarDeclaration name ty <- input ++ output])
            let pres = wlpTree (LitB True) prunedCompTree
            evalZ3 do
                forM pres fromExpr >>= mkAnd >>= mkNot >>= assert
                traceM =<< solverToString
                check >>= \case
                    Sat -> liftIO $ putStrLn "Program invalid"
                    Unsat -> liftIO $ putStrLn "Program valid"
