{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Monad.Writer.Strict
import           Data.Functor
import           Data.Maybe
import qualified Data.Text.IO                as TIO
import           Options.Applicative
import qualified Options.Applicative         as O
import           Prelude                     hiding (FilePath)

import           CabalBrew.Commands
import           CabalBrew.Options
import           CabalBrew.Types


main :: IO ()
main = execParser opts >>= runCabalBrew . cabalBrew . mode >>= outputLogs . snd
    where opts' = Brew <$> subparser (  O.command "install"  installOptions
                                     <> O.command "update"   updateOptions
                                     <> O.command "list"     listOptions
                                     <> O.command "outdated" outdatedOptions
                                     )
          opts  = info (helper <*> opts')
                       (  fullDesc
                       <> progDesc "Manages Haskell executable packages\
                                   \ to be managed by Homebrew."
                       )
          outputLogs = mapM_ TIO.putStrLn

installOptions :: ParserInfo CabalBrew
installOptions = info (helper <*> opts)
                      (  fullDesc
                      <> progDesc "This installs a Haskell program\
                                  \ to be managed by Homebrew."
                      <> header "cabal-brew install - install Haskell\
                                \ packages programs.")
    where opts =   Install
               <$> pnameArg (metavar "NAME"    <> help "The name of the package to install.")
               <*> mverArg  (  metavar "VERSION" <> value Nothing
                            <> help "The version to install."
                            )

updateOptions :: ParserInfo CabalBrew
updateOptions = info (helper <*> opts)
                     (  fullDesc
                     <> progDesc "This updates one or more Haskell programs\
                                 \ managed by Homebrew."
                     <> header "cabal-brew update - update installed Haskell programs.")
    where opts =   Update
               <$> pnameArgs (metavar "PACKAGES" <> help "The name of the packages to install.")

listOptions :: ParserInfo CabalBrew
listOptions = info (helper <*> opts)
                   (  fullDesc
                   <> progDesc "This lists the Cabal packages that Homebrew manages."
                   <> header "cabal-brew list - list cabal-installed,\
                             \ homebrew-managed programs.")
    where opts = pure Ls

outdatedOptions :: ParserInfo CabalBrew
outdatedOptions = info (helper <*> opts)
                       (  fullDesc
                       <> progDesc "This lists the Cabal packages that need updating."
                       <> header "cabal-brew outddated - list outdated cabal-installed,\
                                 \ homebrew-managed programs.")
    where opts = pure Outdated

data BrewOpts = Brew { mode :: CabalBrew } deriving (Show)

