module CC.Coverage.TixData
    ( TixData(..)
    , readTixData
    ) where

import Control.Exception.Safe
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Traversable (for)
import System.FilePath
import System.FilePath.Glob
import Trace.Hpc.Mix
import Trace.Hpc.Tix

data TixData = TixData
    { tdPath :: FilePath
    , tdSource :: Text
    , tdMixEntries :: [MixEntry]
    , tdTix :: [Integer]
    }
    deriving Show

readTixData
    :: FilePath -- ^ Directory for @.mix@ files
    -> FilePath -- ^ Directory for @.tix@ files
    -> Pattern -- ^ How to locate Tix files, e.g. @**/*.tix@
    -> IO (NonEmpty TixData)
readTixData mixDir tixDir p = do
    tixPaths <- globDir1 p tixDir
    tixDatas <- for tixPaths $ \tixPath -> do
        tix <- readTix' tixPath

        for (tixModules tix) $ \tixModule -> do
            mix <- readMix (mixPaths mixDir tixModule) $ Right tixModule
            src <- T.readFile (mixPath mix)

            pure TixData
                { tdPath = mixPath mix
                , tdSource = src
                , tdMixEntries = mixEntries mix
                , tdTix = tixModuleTixs tixModule
                }

    nonEmptyNote "Tix data was empty" $ concat tixDatas

nonEmptyNote :: MonadThrow m => String -> [a] -> m (NonEmpty a)
nonEmptyNote msg = maybe (throwString msg) pure . NE.nonEmpty

readTix' :: FilePath -> IO Tix
readTix' fp =
    maybe (throwString $ "Error reading .tix path: " ++ fp) pure =<< readTix fp

mixPaths :: FilePath -> TixModule -> [FilePath]
mixPaths mixDir tixModule =
    map (\p -> mixDir </> p </> "")
        $ case span (/= '/') (tixModuleName tixModule) of
              (x, []) -> error $ "TixModule name missing '/': " ++ x
              (packageId, _) -> ["", packageId]

mixPath :: Mix -> FilePath
mixPath (Mix fp _ _ _ _) = fp

mixEntries :: Mix -> [MixEntry]
mixEntries (Mix _ _ _ _ es) = es

tixModules :: Tix -> [TixModule]
tixModules (Tix ms) = ms
