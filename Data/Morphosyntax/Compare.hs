module Data.Morphosyntax.Compare
( Stats (..)
, stats
, accuracyLB
, align
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import Data.Char (isSpace)
import Data.List (foldl')

import qualified Data.Morphosyntax.Base as Base
import qualified Data.Morphosyntax.Canonical as Cano
import qualified Data.Morphosyntax.Tagset as Tag

type WordMlt = (Cano.Word, Base.Multi)

orth :: WordMlt -> L.Text
orth = Cano.orth . fst

choice :: WordMlt -> S.Set Tag.Tag
choice = S.fromList . map (Base.tag . fst) . snd

align :: [WordMlt] -> [WordMlt] -> [([WordMlt], [WordMlt])]
align [] [] = []
align [] ys = error "align: null xs, not null ys"
align xs [] = error "align: not null xs, null ys"
align xs ys =
    let (x, y) = match xs ys
    in  (x, y) : align (drop (length x) xs) (drop (length y) ys)
    
match :: [WordMlt] -> [WordMlt] -> ([WordMlt], [WordMlt])
match xs ys =
    doIt 0 xs 0 ys
  where
    doIt i (x:xs) j (y:ys)
        | n == m = ([x], [y])
        | n <  m = x <: doIt n xs j (y:ys)
        | n >  m = y >: doIt i (x:xs) m ys
      where
        n = i + size x
        m = j + size y
    size w = L.length . L.filter (not.isSpace) $ orth w
    x <: (xs, ys) = (x:xs, ys)
    y >: (xs, ys) = (xs, y:ys)

data Stats = Stats
    { good :: Int   -- ^ Number of correct tags
    , gold :: Int } -- ^ Number of segments in gold corpus

(.+.) :: Stats -> Stats -> Stats
Stats x y .+. Stats x' y' = Stats (x + x') (y + y')

accuracyLB :: Stats -> Double
accuracyLB s
    = fromIntegral (good s)
    / fromIntegral (gold s)

-- | Accuracy lower bound. We ignore lemmatization errors.
-- If you want to compute accuracy for two data sets (list
-- of sentences), just concatenate them. Errors on the level
-- of sentence segmentation are not taken on account.
stats :: [WordMlt] -> [WordMlt] -> Stats
stats xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | choice x == choice y = Stats 1 1
        | otherwise = Stats 0 1
    stats xs ys = Stats 0 (length xs)
