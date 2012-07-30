module Data.Morphosyntax.Canonical
( module Data.Morphosyntax.Base
, Word (..)
, word
, choice

, WordDmb
, WordMlt

, Sent
, SentDmb
, SentMlt
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

type WordDmb = (Word, Disamb)
type WordMlt = (Word, Multi)

type Sent       = [Word]
type SentDmb    = [WordDmb]
type SentMlt    = [WordMlt]

word :: (Word, a) -> Word
word = fst

choice :: (Word, b) -> b
choice = snd
