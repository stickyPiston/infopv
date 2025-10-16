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
    , n :: Int
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
            <*> option auto
                ( long "n"
                <> help "The maximum length of a pth"
                <> showDefault
                <> value 100
                <> metavar "INT"
                )
            <*> argument str (metavar "FILE")

main :: IO ()
main = do
    Config { k, n, filepath } <- execParser configParser
    parseGCLfile filepath >>= \case
        Left err -> putStrLn $ "Could not parse " <> filepath <> ": " <> err
        Right Program { stmt, input, output } -> do
            let initialGamma = [(name, ty) | VarDeclaration name ty <- input ++ output]
            let compTree = runReader (buildTree k stmt) initialGamma
            let initialRho = M.fromList [(name, Var (name ++ "_0") ty) | VarDeclaration name ty <- input ++ output]
            (valid, stats) <- evalZ3 do
                true <- mkTrue
                let initialSymbolicState = SymbolicState { environment = initialRho, pathLength = n, constraints = true }
                runSE (verify compTree) initialSymbolicState
            print stats
            putStrLn if valid then "Program valid" else "Program invalid"