{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Shell
    ( brew_
    , cabal_
    ) where


import           Data.Text (Text)
import           Shelly


brew_ :: Text -> [Text] -> Sh ()
brew_ = command1_ "brew" []

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

