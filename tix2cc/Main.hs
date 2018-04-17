{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import CC.Coverage.FileCoverage
import CC.Coverage.GitInfo
import CC.Coverage.Options
import CC.Coverage.Payload
import CC.Coverage.SourceFile
import Control.Exception.Safe
import Control.Monad ((<=<))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import System.Exit (die)
import System.FilePath.Glob
import Trace.Hpc.Tix

main :: IO ()
main = do
    opts@Options {..} <- parseOptions

    handleIO (die . show) $ withinProject opts $ do
        mixDir <- getMixDir opts
        tixDir <- getTixDir opts
        tixPaths <- globDir1 oPattern tixDir
        tixModules <- catTixModules =<< traverse readTixModules tixPaths
        p <- payload
            <$> getGitInfo
            <*> traverse (fromFileCoverage <=< fromTixModule mixDir) tixModules

        BL.putStrLn $ encode p

catTixModules :: MonadThrow m => [[TixModule]] -> m (NonEmpty TixModule)
catTixModules = fromMaybeThrow "Tix data was empty" . NE.nonEmpty . concat

readTixModules :: FilePath -> IO [TixModule]
readTixModules fp = do
    Tix ms <- fromMaybeThrow ("Error reading .tix path: " ++ fp) =<< readTix fp
    pure ms

fromMaybeThrow :: MonadThrow m => String -> Maybe a -> m a
fromMaybeThrow msg = maybe (throwString msg) pure
