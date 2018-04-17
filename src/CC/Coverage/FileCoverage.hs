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
    :: FilePath -- ^ Directory where @.mix@ files can be found
    -> TixModule
    -> IO FileCoverage
fromTixModule mixDir tixModule = do
    mix <- readMix (mixPaths mixDir tixModule) $ Right tixModule
    srcLines <- T.lines <$> T.readFile (mixPath mix)

    let
        hitMap :: Map Natural Integer
        hitMap = Map.fromList
            . map (first $ fromIntegral . fst4 . fromHpcPos . fst)
            $ zip (mixEntries mix) (tixModuleTixs tixModule)

    pure FileCoverage
        { fcPath = mixPath mix
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
