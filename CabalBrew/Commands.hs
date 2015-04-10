{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

module CabalBrew.Commands
    ( cabalBrew
    ) where


import           Control.Applicative
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
        install . cabalPackageInfo packageName
    =<< maybe (getHackageVersion packageName) return packageVersion

cabalBrew (Update []) = do
    packages <- outdated
    case packages of
        [] -> logError "Cabal-brew: Nothing to update."
        ps -> forM_ ps $ \(cbp, v) ->
                log (formatUpgrade cbp v) >> update' cbp v
cabalBrew Update{..} =
    mapM_ updateLog =<< filterM (liftSh . hasPackage) packageNames
    where updateLog n@(PackageName nstr) =
                logSuccess ("Checking " <> T.pack nstr) >> updateName' n

cabalBrew Ls = list >>= printPackages "Installed packages:"

cabalBrew Outdated =   log "Outdated packages:"
                   >>  outdated
                   >>= mapM_ (log . uncurry formatUpgrade)

cabalBrew Remove{..} = logName packageName >> remove packageName
    where logName (PackageName n) = logError $ "Removing " <> T.pack n

-- Slightly more abstract functions. These are the work horses.

install :: CabalBrewPackage -> CabalBrewRun ()
install cbp@CBP{..} = do
    let name@(PackageName nameStr) = cbPackageName
        keg = T.toLower $ "cabal-" <> T.pack nameStr
        versionStr = showVersion cbPackageVersion
        sandbox = cbPackageDirectory FS.</> fromText (T.pack versionStr)

    remove name

    let packageSpec = makePackageSpec name versionStr
    log $ "cabal " <> packageSpec <> " => " <> toTextIgnore sandbox
    liftSh' ("Error installing " <> nameStr) . chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageSpec]
        brew_ "link" ["--overwrite", keg]
    writePackageInfo cbp

updateName :: PackageName -> CabalBrewRun ()
updateName pkgName =
    join $ update <$> readPackageInfo pkgName <*> getHackageVersion pkgName

-- | This is just like update, except it catches errors and just logs them.
updateName' :: PackageName -> CabalBrewRun ()
updateName' pkgName = void . safeCabalBrew Nothing $ updateName pkgName

update :: CabalBrewPackage -> Version -> CabalBrewRun ()
update CBP{..} tov
    | cbPackageVersion < tov =
        let showv = T.pack . showVersion
            pkgName@(PackageName nameStr) = cbPackageName
        in  log (  "Updating " <> T.pack nameStr
                               <> ": " <> showv cbPackageVersion
                <> " => " <> showv tov
                )
            >> cabalBrew (Install pkgName $ Just tov)
    | otherwise = return ()

update' :: CabalBrewPackage -> Version -> CabalBrewRun ()
update' cbp tov =
    void . safeCabalBrew Nothing $ update cbp tov

list :: CabalBrewRun [CabalBrewPackage]
list =
    map PackageName . filter (/= "install")
                    . map (drop 6)
                    . filter (L.isPrefixOf "cabal-")
                    . map (FS.encodeString . FS.filename)
          <$> liftSh (ls cellar) >>=
    mapM readPackageInfo

outdated :: CabalBrewRun [(CabalBrewPackage, Version)]
outdated =
        fmap (mapMaybe foldm . filter cmpv)
    .   mapM decorate
    =<< list
    where decorate cbp =
                fmap (cbp,) <$> getHackageVersion' $ cbPackageName cbp
          cmpv (CBP{..}, Just v1) = cbPackageVersion < v1
          cmpv _                 = False
          foldm (cbp, Just v) = Just (cbp, v)
          foldm (_,  Nothing) = Nothing

remove :: PackageName -> CabalBrewRun ()
remove (PackageName nameStr) = do
    let keg     = T.toLower $ "cabal-" <> T.pack nameStr
        sandbox = cellar FS.</> fromText keg

    whenM (liftSh $ test_d sandbox) $
        liftSh' ("Error removing existing " <> nameStr) $ do
            brew_ "unlink" [keg]
            rm_rf sandbox

-- Utilities

printPackages :: T.Text -> [CabalBrewPackage] -> CabalBrewRun ()
printPackages msg packages = do
    log msg
    mapM_ (log . showBrewPackage) packages

formatUpgrade :: CabalBrewPackage -> Version -> T.Text
formatUpgrade cbp v1 =
    showBrewPackage cbp <> " => " <> T.pack (showVersion v1)

