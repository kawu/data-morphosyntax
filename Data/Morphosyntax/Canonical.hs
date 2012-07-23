module Data.Morphosyntax.Canonical
( Word (..)
, Interp (..)
, Space (..)
, Disamb
, Multi
) where

import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Base
import qualified Data.Morphosyntax.Class as C

data Word = Word
    { orth    :: L.Text
    , space   :: Space
    , interps :: [Interp] }
    deriving (Show, Read, Eq, Ord)

instance C.Morph Word where
    orth    = orth
    space   = space
    interps = interps
