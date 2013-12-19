{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Main where


import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Conduit
import qualified Data.DList                            as D
import           Data.Functor
import           Data.Maybe
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Traversable
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


data Hole = Hole

type PackageName    = Text
type PackageVersion = Text

data CabalBrew = Install { packageName    :: PackageName
                         , packageVersion :: Maybe PackageVersion
                         }
               | Update  { packageNames :: [PackageName]
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
    (out, log) <- runCabalBrew m
    return $ log <$ out

log :: Text -> CabalBrewRun ()
log = liftW . tell . D.singleton

logAll :: [Text] -> CabalBrewRun ()
logAll = liftW . tell . D.fromList

liftSh :: Sh a -> CabalBrewRun a
liftSh = CBR . lift . lift

liftW :: WriterT (D.DList Text) Sh a -> CabalBrewRun a
liftW = CBR . lift

liftET :: EitherT String (WriterT (D.DList Text) Sh) a -> CabalBrewRun a
liftET = CBR

cellar :: FilePath
cellar = FS.concat ["/usr", "local", "Cellar"]

cabalBrew :: CabalBrew -> CabalBrewRun ()
cabalBrew Install{..} = do
    version <- maybe (T.pack . showVersion <$> getHackageVersion packageName)
                    return
                    packageVersion
    let keg     = "cabal-" <> packageName
        sandbox = FS.concat [cellar, fromText keg, fromText version]

    liftSh . whenM (test_d sandbox) $ do
        echo $ "Cleaning out old keg for " <> keg
        brew_ "unlink" [keg]
        rm_rf . (cellar FS.</>) $ fromText keg

    liftSh . chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        echo $ "cabal " <> packageName <> "-" <> version <> " => " <> toTextIgnore sandbox
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" ["-j", packageName <> "-" <> version]
        brew_ "link" ["--overwrite", keg]

cabalBrew (Update []) = do
    packages <-  map (T.drop 6)
             .   filter (T.isPrefixOf "cabal-")
             .   map (toTextIgnore . FS.filename)
             <$> liftSh (ls cellar)
    case packages of
        [] -> liftSh $ echo "Nothing to update."
        ps -> cabalBrew (Update ps)
cabalBrew Update{..} =
    mapM_ update =<< filterM (liftSh . hasPackage) (filter (/= "install") packageNames)

hasPackage :: PackageName -> Sh Bool
hasPackage = test_d . getPackageDirectory

getPackageDirectory :: PackageName -> FilePath
getPackageDirectory = FS.append cellar . fromText . T.append "cabal-"

update :: PackageName -> CabalBrewRun ()
update packageName = do
    v0 <- getCurrentVersion packageName
    v1 <- getHackageVersion packageName
    when (v0 < v1) $ do
        let v1' = showv v1
        echo' $ ">>> Updating " <> packageName <> ": " <> showv v0 <> " => " <> v1'
        cabalBrew . Install packageName $ Just v1'
        echo' ""
    where showv = T.pack . showVersion
          echo' = liftSh . echo

eitherError :: Either String a -> Sh a
eitherError (Right a)  = return a
eitherError (Left msg) = errorExit $ T.pack msg

getCurrentVersion :: PackageName -> CabalBrewRun Version
getCurrentVersion =
        maybeErr . join . fmap (readVersion . FS.encodeString . FS.filename) . listToMaybe
    <=< liftSh . ls . getPackageDirectory
    where maybeErr = liftET . hoistEither . note "Invalid package version."

readVersion :: String -> Maybe Version
readVersion = listToMaybe . map fst . filter (null . snd) . readP_to_S parseVersion

getHackageVersion :: PackageName -> CabalBrewRun Version
getHackageVersion name =
    pkgVersion . package . packageDescription <$> getCabal name

getCabalReq :: PackageName -> IO (Request m)
getCabalReq name =
    parseUrl $ "http://hackage.haskell.org/package/" ++ name' ++ "/" ++ name' ++ ".cabal"
    where name' = T.unpack name

getCabal :: PackageName -> CabalBrewRun GenericPackageDescription
getCabal name = do
    man  <- liftIO $ newManager def
    req  <- liftIO $ getCabalReq name
    resp <-  parsePackageDescription . T.unpack . TE.decodeUtf8 . LBS.toStrict . responseBody
         <$> liftIO (runResourceT . browse man $ makeRequestLbs req)
    liftET $ case resp of
                 ParseOk _ a -> right a
                 ParseFailed e -> left $ show e

brew_ :: Text -> [Text] -> Sh ()
brew_ = command1_ "brew" []

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

main :: IO ()
main = execParser opts >>= void . runCabalBrew . cabalBrew . mode
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
               <$> textArg  (metavar "NAME"    <> help "The name of the package to install.")
               <*> mtextArg (  metavar "VERSION" <> value Nothing
                            <> help "The version to install."
                            )

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

mtextArg :: Mod ArgumentFields (Maybe Text) -> Parser (Maybe Text)
mtextArg = argument (Just . Just . T.pack)

textArgs :: Mod ArgumentFields Text -> Parser [Text]
textArgs = arguments (Just . T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)

