{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Log
    ( log
    , logAll
    ) where


import           Control.Monad.Writer.Strict
import qualified Data.DList                  as D
import           Data.Text                   (Text)

import           CabalBrew.Types


log :: Text -> CabalBrewRun ()
log = liftW . tell . D.singleton

logAll :: [Text] -> CabalBrewRun ()
logAll = liftW . tell . D.fromList

