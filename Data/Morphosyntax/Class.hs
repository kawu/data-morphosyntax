module Data.Morphosyntax.Class
( Morph (..)
, Disamb (..)
) where

import Control.Applicative
import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Base (Interp, Multi, Space)

-- | TODO: Should it provide toCano and fromCano functions?
class Morph w where
    orth    :: w -> L.Text
    space   :: w -> Space
    known   :: w -> Bool
    interps :: w -> [Interp]

class Disamb w where
    disamb  :: w -> Interp

class MultiTag w where
    choice  :: w -> Multi
