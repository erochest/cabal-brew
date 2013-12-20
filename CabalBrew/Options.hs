{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Options
    ( textArg
    , mtextArg
    , textArgs
    , textOption
    , strArg
    , mstrArg
    , strArgs
    , pnameArg
    , pnameArgs
    , verArg
    , mverArg
    ) where


import           Control.Applicative
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Distribution.Package
import           Options.Applicative

import           CabalBrew.Packages
import           CabalBrew.Types


textArg :: Mod ArgumentFields Text -> Parser Text
textArg = argument (Just . T.pack)

mtextArg :: Mod ArgumentFields (Maybe Text) -> Parser (Maybe Text)
mtextArg = argument (Just . Just . T.pack)

textArgs :: Mod ArgumentFields Text -> Parser [Text]
textArgs = arguments (Just . T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)

strArg :: Mod ArgumentFields String -> Parser String
strArg = argument Just

mstrArg :: Mod ArgumentFields (Maybe String) -> Parser (Maybe String)
mstrArg = argument (Just . Just)

strArgs :: Mod ArgumentFields String -> Parser [String]
strArgs = arguments Just

pnameArg :: Mod ArgumentFields PackageName -> Parser PackageName
pnameArg = argument (Just . PackageName)

pnameArgs :: Mod ArgumentFields PackageName -> Parser [PackageName]
pnameArgs = arguments (Just . PackageName)

verArg :: Mod ArgumentFields Version -> Parser Version
verArg = argument readVersion

mverArg :: Mod ArgumentFields (Maybe Version) -> Parser (Maybe Version)
mverArg = argument (Just . readVersion)

