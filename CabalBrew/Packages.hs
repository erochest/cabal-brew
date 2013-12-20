{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Packages
    ( makePackageSpec
    , makePackageSpec'
    , hasPackage
    , getPackageDirectory
    , getCurrentVersion
    , readVersion
    ) where


import           Control.Error
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Version
import           Distribution.Text
import qualified Filesystem.Path.CurrentOS    as FS
import           Prelude                      hiding (FilePath)
import           Shelly
import           Text.ParserCombinators.ReadP

import           CabalBrew.Paths
import           CabalBrew.Types


makePackageSpec :: PackageName -> PackageVersionStr -> T.Text
makePackageSpec n v = T.pack (display n) <> "-" <> T.pack v

makePackageSpec' :: PackageName -> Version -> T.Text
makePackageSpec' n = makePackageSpec n . showVersion

hasPackage :: PackageName -> Sh Bool
hasPackage = test_d . getPackageDirectory

getPackageDirectory :: PackageName -> FilePath
getPackageDirectory = FS.append cellar . FS.decodeString . ("cabal-" ++) . display

getCurrentVersion :: PackageName -> CabalBrewRun Version
getCurrentVersion =
        maybeErr . join . fmap (readVersion . FS.encodeString . FS.filename) . listToMaybe
    <=< liftSh . ls . getPackageDirectory
    where maybeErr = liftET . hoistEither . note "Invalid package version."

readVersion :: String -> Maybe Version
readVersion = listToMaybe . map fst . filter (null . snd) . readP_to_S parseVersion
