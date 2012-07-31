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
    -- | Is the word known on the level of analysis?
    , known   :: Bool
    , interps :: [Interp] }
    deriving (Show, Read, Eq, Ord)

instance C.Morph Word where
    orth    = orth
    space   = space
    known   = known
    interps = interps

type WordDmb = (Word, Disamb)
type WordMlt = (Word, Multi)

word :: (Word, a) -> Word
word = fst

choice :: (Word, b) -> b
choice = snd

type Sent       = [Word]
type SentDmb    = [WordDmb]
type SentMlt    = [WordMlt]
