{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import CC.Coverage
import CC.Coverage.Options
import Control.Exception.Safe (handleIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Exit (die)

main :: IO ()
main = do
    opts@Options {..} <- parseOptions

    handleIO (die . show) $ withinProject opts $ do
        mixDir <- getMixDir opts
        tixDir <- getTixDir opts
        tixData <- readTixData mixDir tixDir oPattern
        payload <-
            Payload <$> getGitInfo <*> traverse tixDataToSourceFile tixData

        BL.putStrLn $ encode payload
