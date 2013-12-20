{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Types
    ( PackageName
    , PackageVersion
    , CabalBrew(..)
    , CabalBrewRun
    , runCabalBrew
    , execCabalBrew
    , logCabalBrew
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
import           Shelly


-- This is for debugging/development only.
data Hole = Hole

type PackageName    = Text
type PackageVersion = Text

data CabalBrew = Install   { packageName    :: PackageName
                           , packageVersion :: Maybe PackageVersion
                           }
               | Update    { packageNames :: [PackageName]
                           }
               | Ls
               | Outdated
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


liftSh :: Sh a -> CabalBrewRun a
liftSh = CBR . lift . lift

liftW :: WriterT (D.DList Text) Sh a -> CabalBrewRun a
liftW = CBR . lift

liftET :: EitherT String (WriterT (D.DList Text) Sh) a -> CabalBrewRun a
liftET = CBR

