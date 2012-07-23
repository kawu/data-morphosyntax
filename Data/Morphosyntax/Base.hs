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

data Interp = Interp
    { base :: L.Text
    , tag  :: Tag }
    deriving (Show, Read, Eq, Ord)

type Disamb = Interp
type Multi  = [(Interp, Double)]    -- ^ Interpretations with probabilities
