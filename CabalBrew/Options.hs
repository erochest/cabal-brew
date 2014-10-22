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
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Distribution.Package
import           Options.Applicative
import           Options.Applicative.Types

import           CabalBrew.Packages
import           CabalBrew.Types


textArg :: Mod ArgumentFields Text -> Parser Text
textArg = argument (reader T.pack)

mtextArg :: Mod ArgumentFields (Maybe Text) -> Parser (Maybe Text)
mtextArg = argument (reader (Just . T.pack))

textArgs :: Mod ArgumentFields Text -> Parser [Text]
textArgs = arguments (reader T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption = option (reader T.pack)

strArg :: Mod ArgumentFields String -> Parser String
strArg = argument readerAsk

mstrArg :: Mod ArgumentFields (Maybe String) -> Parser (Maybe String)
mstrArg = argument (reader Just)

strArgs :: Mod ArgumentFields String -> Parser [String]
strArgs = arguments readerAsk

pnameArg :: Mod ArgumentFields PackageName -> Parser PackageName
pnameArg = argument (reader PackageName)

pnameArgs :: Mod ArgumentFields PackageName -> Parser [PackageName]
pnameArgs = arguments (reader PackageName)

verArg :: Mod ArgumentFields Version -> Parser Version
verArg = argument parseVersion'

mverArg :: Mod ArgumentFields (Maybe Version) -> Parser (Maybe Version)
mverArg = argument parseVersion

parseVersion :: ReadM (Maybe Version)
parseVersion = fmap readVersion readerAsk

parseVersion' :: ReadM Version
parseVersion' = do
    o <- readerAsk
    case readVersion o of
        Just v  -> return v
        Nothing -> readerError ("Invalid version string: " ++ o)

arguments :: ReadM a -> Mod ArgumentFields a -> Parser [a]
arguments r = many . argument r

reader :: (String -> a) -> ReadM a
reader f = f <$> readerAsk

