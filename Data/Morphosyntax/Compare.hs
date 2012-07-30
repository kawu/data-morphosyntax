module Data.Morphosyntax.Compare
( Stats (..)
, weakLB
, weakUB
, strongLB
, strongUB
, accuracy
, align
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import Data.Char (isSpace)
import Data.List (foldl')

import qualified Data.Morphosyntax.Base as Base
import qualified Data.Morphosyntax.Canonical as Cano
import qualified Data.Morphosyntax.Tagset as Tag

type WordMlt = Cano.WordMlt

orth :: Cano.WordMlt -> L.Text
orth = Cano.orth . fst

-- | All tags are expanded here. 
choice :: Tag.Tagset -> WordMlt -> S.Set Tag.Tag
choice tagset
    = S.fromList
    . concatMap (Tag.expand tagset . Base.tag . fst)
    . snd

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

accuracy :: Stats -> Double
accuracy s
    = fromIntegral (good s)
    / fromIntegral (gold s)

weakLB :: Tag.Tagset -> [WordMlt] -> [WordMlt] -> Stats
weakLB tagset xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | S.null (xTags `S.intersection` yTags) = Stats 0 1
        | otherwise = Stats 1 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs ys = Stats 0 (length xs)

strongLB :: Tag.Tagset -> [WordMlt] -> [WordMlt] -> Stats
strongLB tagset xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | xTags == yTags = Stats 1 1
        | otherwise = Stats 0 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs ys = Stats 0 (length xs)

weakUB :: Tag.Tagset -> [WordMlt] -> [WordMlt] -> Stats
weakUB tagset xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | S.null (xTags `S.intersection` yTags) = Stats 0 1
        | otherwise = Stats 1 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs ys = Stats (length xs) (length xs)

strongUB :: Tag.Tagset -> [WordMlt] -> [WordMlt] -> Stats
strongUB tagset xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | xTags == yTags = Stats 1 1
        | otherwise = Stats 0 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs ys = Stats (length xs) (length xs)
