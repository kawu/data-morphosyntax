{-# LANGUAGE RecordWildCards #-}

module Data.Morphosyntax.Base
( Interp (..)
, Space (..)
, Disamb
, Multi
) where

import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Tagset (Tag)

data Space
    = NoSpace
    | Space
    | NewLine
    deriving (Show, Read, Eq, Ord)


