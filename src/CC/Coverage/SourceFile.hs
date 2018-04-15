{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Coverage.SourceFile
    ( SourceFile(..)
    , tixDataToSourceFile
    ) where

import CC.Coverage.Coverage
import CC.Coverage.Git
import CC.Coverage.Sha
import CC.Coverage.Stringly
import CC.Coverage.TixData
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics
import Trace.Hpc.Util

data SourceFile = SourceFile
    { sfName :: FilePath -- ^ Must be Absolute
    , sfBlobId :: Sha
    , sfCoverage :: Stringly [Coverage]
    }
    deriving Generic

instance ToJSON SourceFile where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

tixDataToSourceFile :: TixData -> IO SourceFile
tixDataToSourceFile tixData@TixData {..} = SourceFile
    <$> pure tdPath
    <*> getBlobId tdPath
    <*> pure (Stringly $ tixDataToCoverage tixData)

tixDataToCoverage :: TixData -> [Coverage]
tixDataToCoverage TixData {..} = map lookupCoverage [1 .. lineCount]
  where
    lineCount = length $ T.lines tdSource

    lookupCoverage =
        maybe Uncovered (Covered . fromIntegral) . flip Map.lookup tixMap

    tixMap = Map.fromList . map (first $ fst4 . fromHpcPos . fst) $ zip
        tdMixEntries
        tdTix

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a
