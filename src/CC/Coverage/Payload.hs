{-# LANGUAGE DeriveGeneric #-}

module CC.Coverage.Payload
    ( Payload(..)
    , payload
    ) where

import CC.Coverage.Coverage
import CC.Coverage.GitInfo
import CC.Coverage.Percentage
import CC.Coverage.SourceFile
import CC.Coverage.Stringly
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics

data Payload = Payload
    { pGit :: GitInfo
    , pSourceFiles :: NonEmpty SourceFile
    , pCoveredPercent :: Percentage
    }
    deriving Generic

payload :: GitInfo -> NonEmpty SourceFile -> Payload
payload gitInfo sourceFiles = Payload
    { pGit = gitInfo
    , pSourceFiles = sourceFiles
    , pCoveredPercent = coveredPercent
    }
  where
    coveredPercent = toCoveredPercent
        $ concatMap (unStringly . sfCoverage)
        $ NE.toList sourceFiles

instance ToJSON Payload where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase
