{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Hackage
    ( getHackageVersion
    , getHackageVersion'
    , getCabal
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Network.Wreq

import           CabalBrew.Types


getHackageVersion :: PackageName -> CabalBrewRun Version
getHackageVersion =
    fmap (pkgVersion . package . packageDescription) . getCabal

-- | This is just like getHackageVersion, except it catches errors and just logs them.
getHackageVersion' :: PackageName -> CabalBrewRun (Maybe Version)
getHackageVersion' pname@(PackageName name) =
    safeCabalBrew (Just msg) $ getHackageVersion pname
    where msg = "Error on Hackage: " <> T.pack name

getCabalUrl :: PackageName -> String
getCabalUrl (PackageName name) =
    "http://hackage.haskell.org/package/" ++ name
             ++ "/" ++ name ++ ".cabal"

getCabal :: PackageName -> CabalBrewRun GenericPackageDescription
getCabal name = do
    body <-  parsePackageDescription
         .   T.unpack
         .   TE.decodeUtf8
         .   LBS.toStrict
         .   (^. responseBody)
         <$> liftIO (get $ getCabalUrl name)
    liftET $ case body of
                 ParseOk _ a   -> right a
                 ParseFailed e -> left $ show e
