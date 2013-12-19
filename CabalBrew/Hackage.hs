{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Hackage
    ( getHackageVersion
    , getCabal
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Conduit
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Version
import           Distribution.Package                  hiding (PackageName,
                                                        packageName,
                                                        packageVersion)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit.Browser
import           Shelly

import           CabalBrew.Shell
import           CabalBrew.Types


getHackageVersion :: PackageName -> CabalBrewRun Version
getHackageVersion =
    fmap (pkgVersion . package . packageDescription) . getCabal

getCabalReq :: PackageName -> IO (Request m)
getCabalReq name =
    parseUrl $ "http://hackage.haskell.org/package/" ++ name' ++ "/" ++ name' ++ ".cabal"
    where name' = T.unpack name

getCabal :: PackageName -> CabalBrewRun GenericPackageDescription
getCabal name = do
    man  <- liftIO $ newManager def
    req  <- liftIO $ getCabalReq name
    liftSh . echo $ "Downloading cabal file for " `T.append` name
    resp <-  parsePackageDescription . T.unpack . TE.decodeUtf8 . LBS.toStrict . responseBody
         <$> liftIO' (runResourceT . browse man $ makeRequestLbs req)
    liftET $ case resp of
                 ParseOk _ a -> right a
                 ParseFailed e -> left $ show e

