{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Coverage.StackQuery
    ( buildStackLookup
    ) where

import Control.Exception.Safe (handleIO)
import Data.Aeson hiding (decode)
import qualified Data.ByteString.Char8 as C8
import Data.List (find, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Version
import Data.Yaml (decode)
import System.Process (readProcess)

-- | Use @stack query@ to make a lookup function for local sources
--
-- If we're in the context of a Stack-based monorepo, we need to lookup paths to
-- local dependencies and run Hpc actions (e.g. reading mix files) from within
-- those directories. Using the experimental stack query command, we can find
-- those when needed through this lookup.
--
-- If we're not in such a scenario, this returns a @const Nothing@.
--
buildStackLookup :: IO (String -> Maybe FilePath)
buildStackLookup =
    maybe (const Nothing) (flip lookupPathByPackageId) <$> stackQuery

newtype Query = Query
    { _queryLocals :: Map String PackageInfo
    }

instance FromJSON Query where
    parseJSON = withObject "Query" $ \o -> Query
        <$> o .: "locals"

data PackageInfo = PackageInfo
    { pkgPath :: FilePath
    , pkgVersion :: Version
    }

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> o .: "path"
        <*> o .: "version"

stackQuery :: IO (Maybe Query)
stackQuery = handleIO (const $ pure Nothing)
    $ decode . C8.pack <$> readProcess "stack" ["query"] ""

lookupPathByPackageId :: String -> Query -> Maybe FilePath
lookupPathByPackageId name = (pkgPath <$>) . lookupLocalByPackageId name

lookupLocalByPackageId :: String -> Query -> Maybe PackageInfo
lookupLocalByPackageId name (Query locals) =
    -- The map does me no effing good because my key has a bunch of "-<ID>" crap
    -- that I can't robustly strip.
    snd <$> find (uncurry matchesName) (Map.toList locals)
  where
    matchesName pkgName PackageInfo{..} =
        (pkgName <> "-" <> showVersion pkgVersion) `isPrefixOf` name
