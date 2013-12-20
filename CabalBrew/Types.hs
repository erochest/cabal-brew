{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Types
    ( PackageName(..)
    , PackageVersionStr
    , PackageIdentifier(..)
    , Version(..)
    , CabalBrew(..)
    , CabalBrewRun
    , runCabalBrew
    , execCabalBrew
    , logCabalBrew
    , safeCabalBrew
    , (?>)
    , liftSh
    , liftW
    , liftET
    , Hole
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Strict
import qualified Data.DList                  as D
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Version
import           Distribution.Package
import           Shelly


-- This is for debugging/development only.
data Hole = Hole

type PackageVersionStr = String

data CabalBrew = Install   { packageName    :: PackageName
                           , packageVersion :: Maybe Version
                           }
               | Update    { packageNames :: [PackageName]
                           }
               | Ls
               | Outdated
               | Remove    { packageName :: PackageName
                           }
                deriving (Show)

newtype CabalBrewRun a = CBR { runCBR :: EitherT String (WriterT (D.DList Text) Sh) a }
                       deriving (Monad, Applicative, Functor)

instance MonadIO CabalBrewRun where
    liftIO = liftSh . Shelly.liftIO

runCabalBrew :: (Functor m, MonadIO m) => CabalBrewRun a -> m (Either String a, [Text])
runCabalBrew = fmap (fmap D.toList) . shelly . verbosely . runWriterT . runEitherT . runCBR

execCabalBrew :: (Functor m, MonadIO m) => CabalBrewRun a -> m (Either String a)
execCabalBrew = fmap fst . runCabalBrew

logCabalBrew :: (Functor m, MonadIO m) => CabalBrewRun a -> m (Either String [Text])
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

