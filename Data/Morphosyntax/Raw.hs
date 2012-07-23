module Data.Morphosyntax.Raw
( Word (..)
, Interp (..)
, Disamb
, toCano
, toCanoSent
, toCanoData
) where

import qualified Data.Text.Lazy as L

import qualified Data.Morphosyntax.Canonical as C
import Data.Morphosyntax.Tagset (Tagset, Tag)
import Text.Morphosyntax.Tag (parseTag)

data Word = Word
    { orth    :: L.Text
    , space   :: C.Space
    , interps :: [Interp] }
    deriving (Show, Read, Eq, Ord)

data Interp = Interp
    { base :: L.Text
    , tag  :: L.Text }
    deriving (Show, Read, Eq, Ord)

type Disamb = Interp

toCano :: Tagset -> Word -> C.Word
toCano tagset x =
    C.Word (orth x) (space x) xs
  where
    xs = [ C.Interp (base x) (parseTag tagset $ tag x)
         | x <- interps x ]

toCanoSent :: Tagset -> [Word] -> [C.Word]
toCanoSent tagset = map (toCano tagset)

toCanoData :: Tagset -> [[Word]] -> [[C.Word]]
toCanoData tagset = map (toCanoSent tagset)
