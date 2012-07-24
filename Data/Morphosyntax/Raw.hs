module Data.Morphosyntax.Raw
( Word (..)
, Interp (..)
, Disamb
, Multi
, word
, choice

, Sent
, SentDmb
, SentMlt

, toCanoWord
, toCanoSent
, toCanoSentDmb
, toCanoSentMlt
) where

import qualified Data.Text.Lazy as L

import qualified Data.Morphosyntax.Canonical as Cano
import Data.Morphosyntax.Tagset (Tagset, Tag)
import Text.Morphosyntax.Tag (parseTag)

data Word = Word
    { orth    :: L.Text
    , space   :: Cano.Space
    , interps :: [Interp] }
    deriving (Show, Read, Eq, Ord)

data Interp = Interp
    { base :: L.Text
    , tag  :: L.Text }
    deriving (Show, Read, Eq, Ord)

type Disamb = Interp
type Multi  = [(Interp, Double)]    -- ^ Interpretations with probabilities

type Sent       = [Word]
type SentDmb    = [(Word, Disamb)]
type SentMlt    = [(Word, Multi)]

word :: (Word, a) -> Word
word = fst

choice :: (Word, b) -> b
choice = snd

toCanoIntp :: Tagset -> Interp -> Cano.Interp
toCanoIntp tagset x = Cano.Interp (base x) (parseTag tagset $ tag x)

toCanoWord :: Tagset -> Word -> Cano.Word
toCanoWord tagset x =
    Cano.Word (orth x) (space x) (map (toCanoIntp tagset) (interps x))

toCanoSent :: Tagset -> Sent -> Cano.Sent
toCanoSent tagset = map (toCanoWord tagset)

toCanoSentDmb :: Tagset -> SentDmb -> Cano.SentDmb
toCanoSentDmb tagset xs =
    [ ( toCanoWord tagset x
      , toCanoIntp tagset dmb )
    | (x, dmb) <- xs ]

toCanoSentMlt :: Tagset -> SentMlt -> Cano.SentMlt
toCanoSentMlt tagset xs =
    [ ( toCanoWord tagset x
      , [ (toCanoIntp tagset dmb, prob)
        | (dmb, prob) <- mlt ] )
    | (x, mlt) <- xs ]
