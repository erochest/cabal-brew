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
import           Prelude                    hiding (FilePath, log)
import           Shelly

import           CabalBrew.Hackage
import           CabalBrew.Log
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
    packages <- map fst <$> outdated
    case packages of
        [] -> log "Nothing to update."
        ps -> cabalBrew (Update ps)
cabalBrew Update{..} =
    mapM_ updateLog =<< filterM (liftSh . hasPackage)  packageNames
    where updateLog n = log ("Checking " <> n) >> update' n

cabalBrew Ls =
        log "Installed packages:"
    >>  list
    >>= mapM_ (log . uncurry makePackageSpec')

cabalBrew Outdated =
        log "Outdated packages:"
    >>  outdated
    >>= mapM_ (log . uncurry makePackageSpec')

install :: PackageName -> PackageVersion -> CabalBrewRun ()
install name version = do
    let keg     = T.toLower $ "cabal-" <> name
        sandbox = FS.concat [cellar, fromText keg, fromText version]

    isDir <- liftSh $ test_d sandbox
    when isDir $ do
        log $ "Deleting keg " <> keg
        liftSh' ("Error removing existing " <> T.unpack name) $ do
            brew_ "unlink" [keg]
            rm_rf . (cellar FS.</>) $ fromText keg

    let packageSpec = makePackageSpec name version
    log $ "cabal " <> packageSpec <> " => " <> toTextIgnore sandbox
    liftSh' ("Error installing " <> T.unpack name) . chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageSpec]
        brew_ "link" ["--overwrite", keg]

update :: PackageName -> CabalBrewRun ()
update pkgName = do
    v0 <- getCurrentVersion pkgName
    v1 <- getHackageVersion pkgName
    when (v0 < v1) $ do
        let v1' = showv v1
        log $ "Updating " <> pkgName <> ": " <> showv v0 <> " => " <> v1'
        cabalBrew . Install pkgName $ Just v1'
    where showv = T.pack . showVersion

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

outdated :: CabalBrewRun [(PackageName, Version)]
outdated =
        fmap (map fst . filter cmpv)
    .   mapM decorate
    =<< list
    where decorate p = (p,) <$> getHackageVersion' (fst p)
          cmpv ((_, v0), Just v1) = v0 < v1
          cmpv _                  = False

