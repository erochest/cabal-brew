{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Hackage
    ( getHackageVersion
    , getHackageVersion'
    , getCabal
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Conduit
import qualified Data.DList                            as D
import           Data.Monoid
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

import           CabalBrew.Shell
import           CabalBrew.Types


getHackageVersion :: PackageName -> CabalBrewRun Version
getHackageVersion =
    fmap (pkgVersion . package . packageDescription) . getCabal

-- | This is just like getHackageVersion, except it catches errors and just logs them.
getHackageVersion' :: PackageName -> CabalBrewRun (Maybe Version)
getHackageVersion' name = do
    liftW . pass $ do
        (out, logs) <- liftIO . runCabalBrew $ getHackageVersion name
        case out of
            Left e  -> return (Nothing, (<> D.singleton ("Error on Hackage: " <> name)
                                         <> D.fromList logs
                                         <> D.singleton (T.pack e)))
            Right v -> return (Just v, (<> D.fromList logs))

getCabalReq :: PackageName -> IO (Request m)
getCabalReq name =
    parseUrl $ "http://hackage.haskell.org/package/" ++ name' ++ "/" ++ name' ++ ".cabal"
    where name' = T.unpack name

getCabal :: PackageName -> CabalBrewRun GenericPackageDescription
getCabal name = do
    man  <- liftIO $ newManager def
    req  <- liftIO $ getCabalReq name
    resp <-  parsePackageDescription . T.unpack . TE.decodeUtf8 . LBS.toStrict . responseBody
         <$> liftIO' (runResourceT . browse man $ makeRequestLbs req)
    liftET $ case resp of
                 ParseOk _ a -> right a
                 ParseFailed e -> left $ show e

