{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Options
    ( textArg
    , mtextArg
    , textArgs
    , textOption
    ) where


import           Control.Applicative
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative


textArg :: Mod ArgumentFields Text -> Parser Text
textArg = argument (Just . T.pack)

mtextArg :: Mod ArgumentFields (Maybe Text) -> Parser (Maybe Text)
mtextArg = argument (Just . Just . T.pack)

textArgs :: Mod ArgumentFields Text -> Parser [Text]
textArgs = arguments (Just . T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)

