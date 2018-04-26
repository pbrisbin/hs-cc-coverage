module CC.Coverage.FileCoverage
    ( FileCoverage(..)
    , LineCoverage(..)
    , fromTixModule
    ) where

import CC.Coverage.Coverage
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Numeric.Natural
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import System.FilePath
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

data FileCoverage = FileCoverage
    { fcPath :: FilePath
    , fcLineCoverage :: [LineCoverage]
    }

data LineCoverage = LineCoverage
    { lcNumber :: Natural
    , lcSource :: Text
    , lcCoverage :: Coverage
    }

fromTixModule
    :: (String -> Maybe FilePath)
    -- ^ Alternate source paths by package
    -> FilePath
    -- ^ Relative directory where @.mix@ files can be found
    -> TixModule
    -> IO FileCoverage
fromTixModule lookupLocalSources mixDir tixModule = do
    let localSources = lookupLocalSources
            $ takeWhile (/= '/')
            $ tixModuleName tixModule

    (mix, path) <-
        case localSources of
            Just path -> do
                cwd <- getCurrentDirectory
                withCurrentDirectory path $ do
                    mix <- readMix (mixPaths mixDir tixModule) $ Right tixModule
                    pure (mix, makeRelative cwd $ path </> mixPath mix)
            Nothing -> do
                mix <- readMix (mixPaths mixDir tixModule) $ Right tixModule
                pure (mix, mixPath mix)

    srcLines <- T.lines <$> T.readFile path

    let
        hitMap :: Map Natural Integer
        hitMap = Map.fromList
            . map (first $ fromIntegral . fst4 . fromHpcPos . fst)
            $ zip (mixEntries mix) (tixModuleTixs tixModule)

    pure FileCoverage
        { fcPath = path
        , fcLineCoverage = zipWith (toLineCoverage hitMap) [1 ..] srcLines
        }

toLineCoverage :: Map Natural Integer -> Natural -> Text -> LineCoverage
toLineCoverage hitMap ln ls = LineCoverage ln ls $ lookupCoverage ln hitMap

lookupCoverage :: Natural -> Map Natural Integer -> Coverage
lookupCoverage ln = maybe Ignored (Coverable . fromIntegral) . Map.lookup ln

mixPaths :: FilePath -> TixModule -> [FilePath]
mixPaths mixDir tixModule =
    map (mixDir </>) $ case span (/= '/') (tixModuleName tixModule) of
        (x, []) -> error $ "TixModule name missing '/': " ++ x
        (packageId, _) -> ["", packageId]

mixPath :: Mix -> FilePath
mixPath (Mix fp _ _ _ _) = fp

mixEntries :: Mix -> [MixEntry]
mixEntries (Mix _ _ _ _ es) = es

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a
