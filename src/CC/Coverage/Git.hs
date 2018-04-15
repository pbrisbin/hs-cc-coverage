{-# LANGUAGE OverloadedStrings #-}

module CC.Coverage.Git
    ( getBlobId
    , getBranch
    , getCommitSha
    , getCommittedAt
    ) where

import CC.Coverage.Sha
import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Process

getBlobId :: FilePath -> IO Sha
getBlobId fp = toShaThrow =<< readGit ["hash-object", fp]

getBranch :: IO Text
getBranch = readGit ["rev-parse", "--abbrev-ref", "HEAD"]

getCommitSha :: IO Sha
getCommitSha = toShaThrow =<< readGit ["rev-parse", "HEAD"]

getCommittedAt :: IO UTCTime
getCommittedAt = do
    output <- readGit ["rev-list", "--format=%at", "--max-count=1", "HEAD"]

    case T.lines output of
        [] -> throwString "git rev-list returned no output"
        ls -> parseUnixSeconds $ T.unpack $ last ls

readGit :: [String] -> IO Text
readGit args = T.strip . T.pack <$> readProcess "git" args ""

parseUnixSeconds :: String -> IO UTCTime
parseUnixSeconds = parseTimeM False defaultTimeLocale "%s"
