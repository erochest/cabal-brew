{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module CabalBrew.Paths
    ( cellar
    ) where


import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, concat)


cellar :: FilePath
cellar = concat ["/usr", "local", "Cellar"]


