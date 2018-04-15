{-# LANGUAGE DeriveGeneric #-}

module CC.Coverage.GitInfo
    ( GitInfo(..)
    , getGitInfo
    ) where

import CC.Coverage.Git
import CC.Coverage.Sha
import CC.Coverage.Timestamp
import Data.Aeson
import Data.Aeson.Casing
import Data.Text (Text)
import GHC.Generics

data GitInfo = GitInfo
    { giBranch :: Text
    , giCommittedAt :: Timestamp
    , giHead :: Sha
    }
    deriving Generic

instance ToJSON GitInfo where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

getGitInfo :: IO GitInfo
getGitInfo = GitInfo
    <$> getBranch
    <*> (Timestamp <$> getCommittedAt)
    <*> getCommitSha
