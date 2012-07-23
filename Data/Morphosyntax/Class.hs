module Data.Morphosyntax.Class
( Morph (..)
, Disamb (..)
, known
) where

import Control.Applicative
import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Base (Interp, Multi, Space)

-- | TODO: Should it provide toCano and fromCano functions?
class Morph w where
    orth    :: w -> L.Text
    space   :: w -> Space
    interps :: w -> [Interp]

known :: Morph w => w -> Bool
known = not . null . interps

class Disamb w where
    disamb  :: w -> Interp

class MultiTag w where
    choice  :: w -> Multi
