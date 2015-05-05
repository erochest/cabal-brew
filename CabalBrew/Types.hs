{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module CabalBrew.Types
    ( PackageName(..)
    , PackageVersionStr
    , PackageIdentifier(..)
    , Version(..)
    , CabalBrew(..)
    , CabalBrewRun
    , CabalBrewPackage(..)
    , runCabalBrew
    , execCabalBrew
    , logCabalBrew
    , safeCabalBrew
    , (?>)
    , liftSh
    , liftW
    , liftET
    ) where


import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Strict
import           Data.Aeson
import qualified Data.DList                  as D
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Version
import           Distribution.Package
import qualified Filesystem.Path.CurrentOS   as FS
import           Shelly


type PackageVersionStr = String

data CabalBrew = Install   { packageName    :: PackageName
                           , packageVersion :: Maybe Version
                           , installFlags   :: Maybe T.Text
                           }
               | Update    { packageNames :: [PackageName]
                           }
               | Ls
               | Outdated
               | Remove    { packageName :: PackageName
                           }
                deriving (Show)

data CabalBrewPackage = CBP
                      { cbPackageName      :: PackageName
                      , cbPackageVersion   :: Version
                      , cbPackageDirectory :: FS.FilePath
                      } deriving (Show)

instance ToJSON Version where
    toJSON Version{..} = object
                       [ "branch" .= versionBranch
                       , "tags"   .= versionTags
                       ]

instance ToJSON CabalBrewPackage where
    toJSON CBP{..} = object
                   [ "name"    .= pName
                   , "version" .= toJSON cbPackageVersion
                   , "path"    .= FS.encodeString cbPackageDirectory
                   ]
                   where (PackageName pName) = cbPackageName

instance FromJSON Version where
    parseJSON (Object obj) =   Version
                           <$> obj .: "branch"
                           <*> obj .: "tags"
    parseJSON _            = mzero

instance FromJSON CabalBrewPackage where
    parseJSON (Object obj) =   CBP
                           <$> fmap PackageName (obj .: "name")
                           <*> obj .: "version"
                           <*> fmap FS.decodeString (obj .: "path")

    parseJSON _            = mzero

newtype CabalBrewRun a
    = CBR { runCBR :: EitherT String (WriterT (D.DList Text) Sh) a }
    deriving (Monad, Applicative, Functor)

instance MonadIO CabalBrewRun where
    liftIO = liftSh . Shelly.liftIO

runCabalBrew :: (Functor m, MonadIO m)
             => CabalBrewRun a -> m (Either String a, [Text])
runCabalBrew = fmap (fmap D.toList) . shelly . verbosely
             . runWriterT . runEitherT . runCBR

execCabalBrew :: (Functor m, MonadIO m) => CabalBrewRun a -> m (Either String a)
execCabalBrew = fmap fst . runCabalBrew

logCabalBrew :: (Functor m, MonadIO m)
              => CabalBrewRun a -> m (Either String [Text])
logCabalBrew m = do
    (a, w) <- runCabalBrew m
    return $ w <$ a

safeCabalBrew :: Maybe T.Text -> CabalBrewRun a -> CabalBrewRun (Maybe a)
safeCabalBrew errMsg m = liftW . pass $ do
    (out, logs) <- liftIO $ runCabalBrew m
    case out of
        Right v -> return (Just v, (`D.append` D.fromList logs))
        Left e  -> let logs'   = D.fromList logs `D.snoc` T.pack e
                       withErr = maybe logs' (`D.cons` logs') errMsg
                   in  return (Nothing, (`D.append` withErr))

(?>) :: CabalBrewRun a -> (a -> CabalBrewRun b) -> CabalBrewRun (Maybe b)
m ?> n = maybe (return Nothing) (fmap Just . n) =<< safeCabalBrew Nothing m


liftSh :: Sh a -> CabalBrewRun a
liftSh = CBR . lift . lift

liftW :: WriterT (D.DList Text) Sh a -> CabalBrewRun a
liftW = CBR . lift

liftET :: EitherT String (WriterT (D.DList Text) Sh) a -> CabalBrewRun a
liftET = CBR

