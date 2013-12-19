{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

module CabalBrew.Commands
    ( cabalBrew
    ) where


import           Control.Monad
import           Control.Monad.Writer.Class
import qualified Data.DList                 as D
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Version
import qualified Filesystem.Path.CurrentOS  as FS
import           Prelude                    hiding (FilePath)
import           Shelly

import           CabalBrew.Hackage
import           CabalBrew.Packages
import           CabalBrew.Paths
import           CabalBrew.Shell
import           CabalBrew.Types


cabalBrew :: CabalBrew -> CabalBrewRun ()
cabalBrew Install{..} =
    maybe (T.pack . showVersion <$> getHackageVersion packageName)
          return
          packageVersion >>=
    install packageName

cabalBrew (Update []) = do
    packages <- map fst <$> list
    case packages of
        [] -> liftSh $ echo "Nothing to update."
        ps -> cabalBrew (Update ps)
cabalBrew Update{..} =
    mapM_ update' =<< filterM (liftSh . hasPackage)  packageNames

cabalBrew Ls =
    mapM_ (liftSh . echo . uncurry makePackageSpec') =<< list

install :: PackageName -> PackageVersion -> CabalBrewRun ()
install name version = do
    let keg     = T.toLower $ "cabal-" <> name
        sandbox = FS.concat [cellar, fromText keg, fromText version]

    liftSh' ("Error removing existing " <> T.unpack name)  . whenM (test_d sandbox) $ do
        echo $ "Cleaning out old keg for " <> keg
        brew_ "unlink" [keg]
        rm_rf . (cellar FS.</>) $ fromText keg

    liftSh' ("Error installing " <> T.unpack name) . chdir "/tmp" $ do
        let packageSpec = makePackageSpec name version
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        echo $ "cabal " <> packageSpec <> " => " <> toTextIgnore sandbox
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageSpec]
        brew_ "link" ["--overwrite", keg]

update :: PackageName -> CabalBrewRun ()
update pkgName = do
    v0 <- getCurrentVersion pkgName
    v1 <- getHackageVersion pkgName
    when (v0 < v1) $ do
        let v1' = showv v1
        echo' $ ">>> Updating " <> pkgName <> ": " <> showv v0 <> " => " <> v1'
        cabalBrew . Install pkgName $ Just v1'
        echo' ""
    where showv = T.pack . showVersion
          echo' = liftSh . echo

-- | This is just like update, except it catches errors and just logs them.
update' :: PackageName -> CabalBrewRun ()
update' pkgName = liftW . pass $ do
    (out, logs) <- liftIO . runCabalBrew $ update pkgName
    case out of
        Left err -> return ((), (<> D.fromList logs <> D.singleton (T.pack err)))
        Right _  -> return ((), (<> D.fromList logs))

list :: CabalBrewRun [(PackageName, Version)]
list =
    filter (/= "install") . map (T.drop 6)
                         . filter (T.isPrefixOf "cabal-")
                         . map (toTextIgnore . FS.filename)
          <$> liftSh (ls cellar) >>=
    mapM (\n -> (n,) <$> getCurrentVersion n)

