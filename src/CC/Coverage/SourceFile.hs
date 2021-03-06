{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Coverage.SourceFile
    ( SourceFile(..)
    , fromFileCoverage
    ) where

import CC.Coverage.Coverage
import CC.Coverage.FileCoverage
import CC.Coverage.Git
import CC.Coverage.Percentage
import CC.Coverage.Sha
import CC.Coverage.Stringly
import Data.Aeson
import Data.Aeson.Casing
import Data.Semigroup ((<>))
import GHC.Generics

data SourceFile = SourceFile
    { sfName :: FilePath
    , sfBlobId :: Sha
    , sfCoveredPercent :: Percentage
    , sfCoverage :: Stringly [Coverage]
    }
    deriving Generic

instance ToJSON SourceFile where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

fromFileCoverage :: String -> FileCoverage -> IO SourceFile
fromFileCoverage prefix FileCoverage{..} = do
    let coverage = map lcCoverage fcLineCoverage

    SourceFile
        <$> pure (prefix <> fcPath)
        <*> getBlobId fcPath
        <*> pure (toCoveredPercent coverage)
        <*> pure (Stringly coverage)
