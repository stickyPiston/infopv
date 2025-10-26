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

import Verifier.Checker
import System.Random (initStdGen)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

data Config = Config
    { k :: Int
    , n :: Int
    , ph :: PruneHeuristic
    , p :: Bool
    , se :: Bool
    , filepath :: String
    }

configParser :: ParserInfo Config
configParser = info parser fullDesc
    where
        parser = Config
            <$> option auto
                ( long "k"
                <> short 'k'
                <> help "The number of times a while loop is unrolled"
                <> showDefault
                <> value 10
                <> metavar "INT"
                )
            <*> option auto
                ( long "n"
                <> short 'n'
                <> help "The maximum length of a pth"
                <> showDefault
                <> value 100
                <> metavar "INT"
                )
            <*> option auto
                ( long "ph"
                <> help "Which heuristic the pruning should abide by"
                <> showDefault
                <> value LengthBased
                <> metavar "HEURISTIC"
                )
            <*> switch
                ( long "print-tree"
                <> short 'p'
                <> help "Print computation tree"
                )
            <*> (not <$> switch
                ( long "no-simplify"
                <> help "Flag to disable simplification"
                ))
            <*> argument str (metavar "FILE")

main :: IO ()
main = do
    setLocaleEncoding utf8
    Config { k, n, ph, p, se, filepath } <- execParser configParser
    parseGCLfile filepath >>= \case
        Left err -> putStrLn $ "Could not parse " <> filepath <> ": " <> err
        Right program -> do
            (valid, stats) <- verifyProgram k n ph p se program
            print stats
            putStrLn if valid then "Program valid" else "Program invalid"
