{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Shell
    ( liftSh'
    , liftIO'
    , brew_
    , cabal_
    ) where


import           Control.Error
import           Control.Exception (SomeException, try)
import           Data.Text         (Text)
import           Shelly

import           CabalBrew.Types


brew_ :: Text -> [Text] -> Sh ()
brew_ = command1_ "brew" []

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

liftSh' :: String -> Sh a -> CabalBrewRun a
liftSh' errmsg s = do
    retValue <- liftSh . errExit False $ s
    exitCode <- liftSh lastExitCode
    if exitCode == 0
        then liftET $ right retValue
        else liftET $ left errmsg

liftIO' :: IO a -> CabalBrewRun a
liftIO' io = do
    e <- fmapL show <$> liftIO (try' io)
    liftET $ hoistEither e
    where try' :: IO a -> IO (Either SomeException a)
          try' = try

