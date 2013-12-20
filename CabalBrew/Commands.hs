{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

module CabalBrew.Commands
    ( cabalBrew
    ) where


import           Control.Monad
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Version
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath, log)
import           Shelly

import           CabalBrew.Hackage
import           CabalBrew.Log
import           CabalBrew.Packages
import           CabalBrew.Paths
import           CabalBrew.Shell
import           CabalBrew.Types


-- The main controller

cabalBrew :: CabalBrew -> CabalBrewRun ()
cabalBrew Install{..} =
        install packageName
    =<< maybe (getHackageVersion packageName) return packageVersion

cabalBrew (Update []) = do
    packages <- outdated
    case packages of
        [] -> log "Nothing to update."
        ps -> forM_ ps $ \pkg@(n, v0, v1) ->
                log (formatUpgrade pkg) >> update' n v0 v1
cabalBrew Update{..} =
    mapM_ updateLog =<< filterM (liftSh . hasPackage) packageNames
    where updateLog n@(PackageName nstr) =
                log ("Checking " <> T.pack nstr) >> updateName' n

cabalBrew Ls = list >>= printPackages "Installed packages:"

cabalBrew Outdated = do
        log "Outdated packages:"
    >>  outdated
    >>= mapM_ (log . formatUpgrade)

-- Slightly more abstract functions. These are the work horses.

install :: PackageName -> Version -> CabalBrewRun ()
install name@(PackageName nameStr) version = do
    let keg     = T.toLower $ "cabal-" <> T.pack nameStr
        sandbox = FS.concat [cellar, fromText keg, fromText . T.pack $ showVersion version]

    isDir <- liftSh $ test_d sandbox
    when isDir $ do
        log $ "Deleting keg " <> keg
        liftSh' ("Error removing existing " <> nameStr) $ do
            brew_ "unlink" [keg]
            rm_rf . (cellar FS.</>) $ fromText keg

    let packageSpec = makePackageSpec name $ showVersion version
    log $ "cabal " <> packageSpec <> " => " <> toTextIgnore sandbox
    liftSh' ("Error installing " <> nameStr) . chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageSpec]
        brew_ "link" ["--overwrite", keg]

updateName :: PackageName -> CabalBrewRun ()
updateName pkgName = do
    v0 <- getCurrentVersion pkgName
    v1 <- getHackageVersion pkgName
    update pkgName v0 v1

-- | This is just like update, except it catches errors and just logs them.
updateName' :: PackageName -> CabalBrewRun ()
updateName' pkgName = void . safeCabalBrew Nothing $ updateName pkgName

update :: PackageName -> Version -> Version -> CabalBrewRun ()
update pkgName@(PackageName nameStr) fromv tov
    | fromv < tov =
        let showv = T.pack . showVersion
        in  log (  "Updating " <> T.pack nameStr <> ": " <> showv fromv
                <> " => " <> showv tov
                )
            >> cabalBrew (Install pkgName $ Just tov)
    | otherwise = return ()

update' :: PackageName -> Version -> Version -> CabalBrewRun ()
update' pkgName fromv tov =
    void . safeCabalBrew Nothing $ update pkgName fromv tov

list :: CabalBrewRun [(PackageName, Version)]
list =
    map PackageName . filter (/= "install")
                    . map (drop 6)
                    . filter (L.isPrefixOf "cabal-")
                    . map (FS.encodeString . FS.filename)
          <$> liftSh (ls cellar) >>=
    mapM (\n -> (n,) <$> getCurrentVersion n)

outdated :: CabalBrewRun [(PackageName, Version, Version)]
outdated =
        fmap (mapMaybe foldm . filter cmpv)
    .   mapM decorate
    =<< list
    where decorate (n, v) = do
                v' <- getHackageVersion' n
                return (n, v, v')
          cmpv (_, v0, Just v1) = v0 < v1
          cmpv _                = False
          foldm (n, v0, Just v1) = Just (n, v0, v1)
          foldm (_, _,  Nothing) = Nothing

-- Utilities

printPackages :: T.Text -> [(PackageName, Version)] -> CabalBrewRun ()
printPackages msg packages = do
    log msg
    mapM_ (log . uncurry makePackageSpec') packages

formatUpgrade :: (PackageName, Version, Version) -> T.Text
formatUpgrade (n, v0, v1) =
    makePackageSpec' n v0 <> " => " <> T.pack (showVersion v1)

