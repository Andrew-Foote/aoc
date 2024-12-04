{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_aoc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "aoc"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Advent of Code solutions"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
