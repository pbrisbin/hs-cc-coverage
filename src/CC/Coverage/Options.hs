{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Coverage.Options
    ( Options(..)
    , parseOptions
    , withinProject
    , getMixDir
    , getTixDir
    ) where

import Control.Applicative
import Control.Exception.Safe
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory (withCurrentDirectory)
import System.FilePath
import System.FilePath.Glob
import System.Process

data Options = Options
    { oMixDir :: Maybe FilePath
    , oTixDir :: Maybe FilePath
    , oDirectory :: Maybe FilePath
    , oPattern :: Pattern
    }

parseOptions :: IO Options
parseOptions = execParser
    $ info (helper <*> parser)
    $ fullDesc <> progDesc "Create Code Climate coverage files from HPC output"

parser :: Parser Options
parser = Options
    <$> optional (strOption
        (  short 'm'
        <> long "mix-dir"
        <> metavar "DIR"
        <> help "Directory for .mix files, inferred for Stack or dist/hpc/mix"
        ))
    <*> optional (strOption
        (  short 't'
        <> long "tix-dir"
        <> metavar "DIR"
        <> help "Directory for .tix files, inferred for Stack or dist/hpc/tix"
        ))
    <*> optional (strOption
        (  short 'C'
        <> long "directory"
        <> metavar "DIR"
        <> help "Directory to run within, default is current"
        ))
    <*> (compile <$> argument str
        (  metavar "PATTERN"
        <> help "Pattern used to locate .tix files, default is **/*.tix"
        <> value "**/*.tix"
        ))

withinProject :: Options -> IO () -> IO ()
withinProject Options {..} =
    maybe id withCurrentDirectory oDirectory

getMixDir :: Options -> IO FilePath
getMixDir Options {..} = do
    mStackMixDir <- getStackMixDir
    pure $ fromMaybe defaultMixDir $ oMixDir <|> mStackMixDir

getTixDir :: Options -> IO FilePath
getTixDir Options {..} = do
    mStackTixDir <- getStackTixDir
    pure $ fromMaybe defaultTixDir $ oTixDir <|> mStackTixDir

getStackMixDir :: IO (Maybe FilePath)
getStackMixDir = handleAsNothing $ (</> "hpc") . chomp <$> readProcess
    "stack"
    ["path", "--dist-dir"]
    ""

getStackTixDir :: IO (Maybe FilePath)
getStackTixDir = handleAsNothing $ chomp <$> readProcess
    "stack"
    ["path", "--local-hpc-root"]
    ""

handleAsNothing :: IO a -> IO (Maybe a)
handleAsNothing = handleIO (const $ pure Nothing) . (Just <$>)

defaultMixDir :: FilePath
defaultMixDir = "dist" </> "hpc" </> "mix"

defaultTixDir :: FilePath
defaultTixDir = "dist" </> "hpc" </> "tix"

chomp :: String -> String
chomp = reverse . dropWhile (== '\n') . reverse
