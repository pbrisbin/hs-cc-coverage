{-# LANGUAGE DeriveGeneric #-}

module CC.Coverage.Payload
    ( Payload(..)
    ) where

import CC.Coverage.GitInfo
import CC.Coverage.SourceFile
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

data Payload = Payload
    { pGit :: GitInfo
    , pSourceFiles :: NonEmpty SourceFile
    }
    deriving Generic

instance ToJSON Payload where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase
