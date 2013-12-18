{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- TODO: Break into its own project.
-- TODO: Lookup version on install.
-- TODO: Use an `InstalledProject` data type.
-- TODO: Use `EitherT String Sh` more consistently.
-- TODO: Use `WriterT (EitherT String Sh)` for summary of what happened.
-- TODO: Consider a library other than `Shelly` for running shell commands. That would remove a dependency, and a fairly intrusive one, too.
-- TODO: `list` command.
-- TODO: Continue upgrading if one package fails.
-- TODO: Print summary of actions at the end.
-- TODO: `outdated` command.
-- TODO: Print all outdated packages before upgrading any.


module Main where


import           Control.Error
import           Control.Monad
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Conduit
import           Data.Maybe
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Version
import           Distribution.Package                  hiding (PackageName)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import qualified Filesystem.Path.CurrentOS             as FS
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit.Browser
import           Options.Applicative
import qualified Options.Applicative                   as O
import           Prelude                               hiding (FilePath)
import           Shelly
import           Text.ParserCombinators.ReadP


type PackageName    = Text
type PackageVersion = Text

data CabalBrew = Install { packageName    :: PackageName
                         , packageVersion :: PackageVersion
                         }
               | Update  { packageNames :: [PackageName]
                         }
                deriving (Show)

cellar :: FilePath
cellar = FS.concat ["/usr", "local", "Cellar"]

cabalBrew :: CabalBrew -> Sh ()
cabalBrew Install{..} = do
    whenM (test_d sandbox) $ do
        echo $ "Cleaning out old keg for " <> keg
        brew_ "unlink" [keg]
        rm_rf . (cellar FS.</>) $ fromText keg
    chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        echo $ "cabal " <> packageName <> "-" <> packageVersion <> " => " <> toTextIgnore sandbox
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageName <> "-" <> packageVersion]
        brew_ "link" ["--overwrite", keg]
    where keg     = "cabal-" <> packageName
          sandbox = FS.concat [ cellar
                              , fromText keg
                              , fromText packageVersion
                              ]

cabalBrew (Update []) = do
    packages <-  map (T.drop 6)
             .   filter (T.isPrefixOf "cabal-")
             .   map (toTextIgnore . FS.filename)
             <$> ls cellar
    case packages of
        [] -> echo "Nothing to update."
        ps -> cabalBrew (Update ps)
cabalBrew Update{..} =
    mapM_ update =<< filterM hasPackage (filter (/= "install") packageNames)

hasPackage :: PackageName -> Sh Bool
hasPackage = test_d . getPackageDirectory

getPackageDirectory :: PackageName -> FilePath
getPackageDirectory = FS.append cellar . fromText . T.append "cabal-"

update :: PackageName -> Sh ()
update packageName = eitherError =<< (runEitherT $ do
    v0 <-  EitherT $ note "Invalid or missing current package."
       <$> getCurrentVersion packageName
    v1 <- EitherT $ getHackageVersion packageName
    if v0 < v1
        then do
            let v1' = showv v1
            echo' $ ">>> Updating " <> packageName <> ": " <> (showv v0) <> " => " <> v1'
            lift . cabalBrew $ Install packageName v1'
            echo' ""
        else return ())
    where showv = T.pack . showVersion
          lift  = EitherT . fmap Right
          echo' = lift . echo

eitherError :: Either String a -> Sh a
eitherError (Right a)  = return a
eitherError (Left msg) = errorExit $ T.pack msg

getCurrentVersion :: PackageName -> Sh (Maybe Version)
getCurrentVersion name =
        join . fmap (readVersion . FS.encodeString . FS.filename) . listToMaybe
    <$> ls (getPackageDirectory name)

readVersion :: String -> Maybe Version
readVersion = listToMaybe . map fst . filter (null . snd) . readP_to_S parseVersion

getHackageVersion :: PackageName -> Sh (Either String Version)
getHackageVersion name =
    liftIO $ fmap (pkgVersion . package . packageDescription) <$> getCabal name

getCabalReq :: PackageName -> IO (Request m)
getCabalReq name =
    parseUrl $ "http://hackage.haskell.org/package/" ++ name' ++ "/" ++ name' ++ ".cabal"
    where name' = T.unpack name

getCabal :: PackageName -> IO (Either String GenericPackageDescription)
getCabal name = do
    man  <- newManager def
    req  <- getCabalReq name
    resp <-  parsePackageDescription . T.unpack . TE.decodeUtf8 . LBS.toStrict . responseBody
         <$> (runResourceT $ browse man $ makeRequestLbs req)
    return $ case resp of
                 ParseOk _ a -> Right a
                 ParseFailed e -> Left $ show e

brew_ :: Text -> [Text] -> Sh ()
brew_ = command1_ "brew" []

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

main :: IO ()
main = execParser opts >>= shelly . verbosely . cabalBrew . mode
    where opts' = Brew <$> subparser (  O.command "install" installOptions
                                     <> O.command "update"  updateOptions
                                     )
          opts  = info (helper <*> opts')
                       (  fullDesc
                       <> progDesc "Manages Haskell executable packages\
                                   \ to be managed by Homebrew."
                       )

installOptions = info (helper <*> opts)
                      (  fullDesc
                      <> progDesc "This installs a Haskell program\
                                  \ to be managed by Homebrew."
                      <> header "cabal-brew install - install Haskell\
                                \ packages programs.")
    where opts =   Install
               <$> textArg (metavar "NAME"    <> help "The name of the package to install.")
               <*> textArg (metavar "VERSION" <> help "The version to install.")

updateOptions = info (helper <*> opts)
                     (  fullDesc
                     <> progDesc "This updates one or more Haskell programs\
                                 \ managed by Homebrew."
                     <> header "cabal-brew update - update installed Haskell programs.")
    where opts =   Update
               <$> textArgs (metavar "PACKAGES" <> help "The name of the packages to install.")

data BrewOpts = Brew { mode :: CabalBrew } deriving (Show)

textArg :: Mod ArgumentFields Text -> Parser Text
textArg = argument (Just . T.pack)

textArgs :: Mod ArgumentFields Text -> Parser [Text]
textArgs = arguments (Just . T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)

