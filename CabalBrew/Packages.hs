{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Packages
    ( makePackageSpec
    , makePackageSpec'
    , showBrewPackage
    , hasPackage
    , getPackageDirectory
    , cabalPackageInfo
    , writePackageInfo
    , readPackageInfo
    , getCurrentVersion
    , readVersion
    ) where


import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy         as BS
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

showBrewPackage :: CabalBrewPackage -> T.Text
showBrewPackage CBP{..} = makePackageSpec' cbPackageName cbPackageVersion

hasPackage :: PackageName -> Sh Bool
hasPackage = test_d . getPackageDirectory

getPackageDirectory :: PackageName -> FilePath
getPackageDirectory = FS.append cellar . FS.decodeString . ("cabal-" ++) . display

cabalPackageInfo :: PackageName -> Version -> CabalBrewPackage
cabalPackageInfo name@(PackageName nameStr) version =
    CBP name version . (cellar FS.</>) . FS.decodeString $ "cabal-" ++ nameStr

writePackageInfo :: CabalBrewPackage -> CabalBrewRun ()
writePackageInfo cbp =
    liftIO . BS.writeFile (FS.encodeString filename) $ Data.Aeson.encode cbp
    where filename = cbPackageDirectory cbp FS.</> "cabal-brew.json"

readPackageInfo :: PackageName -> CabalBrewRun CabalBrewPackage
readPackageInfo name = do
    exists <- liftSh $ test_f filename
    if exists
        then liftET . hoistEither . Data.Aeson.eitherDecode =<< liftIO (BS.readFile filename')
        else cabalPackageInfo name <$> getCurrentVersion name
    where filename  = getPackageDirectory name FS.</> "cabal-brew.json"
          filename' = FS.encodeString filename

getCurrentVersion :: PackageName -> CabalBrewRun Version
getCurrentVersion =
        maybeErr . join . fmap (readVersion . FS.encodeString . FS.filename) . listToMaybe
    <=< liftSh . ls . getPackageDirectory
    where maybeErr = liftET . hoistEither . note "Invalid package version."

readVersion :: String -> Maybe Version
readVersion = listToMaybe . map fst . filter (null . snd) . readP_to_S parseVersion
