{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

module CabalBrew.Commands
    ( cabalBrew
    ) where


import           Control.Monad
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Version
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)
import           Shelly

import           CabalBrew.Hackage
import           CabalBrew.Packages
import           CabalBrew.Paths
import           CabalBrew.Shell
import           CabalBrew.Types


cabalBrew :: CabalBrew -> CabalBrewRun ()
cabalBrew Install{..} = do
    version <- maybe (T.pack . showVersion <$> getHackageVersion packageName)
                    return
                    packageVersion
    let keg     = T.toLower $ "cabal-" <> packageName
        sandbox = FS.concat [cellar, fromText keg, fromText version]

    liftSh . whenM (test_d sandbox) $ do
        echo $ "Cleaning out old keg for " <> keg
        brew_ "unlink" [keg]
        rm_rf . (cellar FS.</>) $ fromText keg

    liftSh . chdir "/tmp" $ do
        let packageSpec = makePackageSpec packageName version
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        echo $ "cabal " <> packageSpec <> " => " <> toTextIgnore sandbox
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageSpec]
        brew_ "link" ["--overwrite", keg]

cabalBrew (Update []) = do
    packages <- map fst <$> listInstalled
    case packages of
        [] -> liftSh $ echo "Nothing to update."
        ps -> cabalBrew (Update ps)
cabalBrew Update{..} =
    mapM_ update =<< filterM (liftSh . hasPackage)  packageNames

cabalBrew Ls =
    mapM_ (liftSh . echo . uncurry makePackageSpec') =<< listInstalled

listInstalled :: CabalBrewRun [(PackageName, Version)]
listInstalled =
    filter (/= "install") . map (T.drop 6)
                         . filter (T.isPrefixOf "cabal-")
                         . map (toTextIgnore . FS.filename)
          <$> liftSh (ls cellar) >>=
    mapM (\n -> (n,) <$> getCurrentVersion n)

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

