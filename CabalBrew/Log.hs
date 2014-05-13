{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Log
    ( log
    , logAll
    , logError
    , logSuccess
    , inColor
    ) where


import           Control.Monad.Writer.Strict
import qualified Data.DList                  as D
import           Data.Text                   (Text, pack)
import           Prelude                     hiding (log)
import           System.Console.ANSI

import           CabalBrew.Types


log :: Text -> CabalBrewRun ()
log = liftW . tell . D.singleton

logAll :: [Text] -> CabalBrewRun ()
logAll = liftW . tell . D.fromList

logError :: Text -> CabalBrewRun()
logError = log . inColor Red

logSuccess :: Text -> CabalBrewRun ()
logSuccess = log . inColor Green

inColor :: Color -> Text -> Text
inColor c t = pack (setSGRCode sgr) <> t <> pack (setSGRCode [Reset])
    where sgr = [SetColor Foreground Dull c]
