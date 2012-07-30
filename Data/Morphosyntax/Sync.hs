{-# LANGUAGE PatternGuards #-}

module Data.Morphosyntax.Sync
( sync
) where

import qualified Data.Text.Lazy as L
import Control.Applicative ((<$>))
import Control.Monad.Writer
import Data.List (find)

import Data.Morphosyntax.Base
import Data.Morphosyntax.Canonical
import Data.Morphosyntax.Compare (align)
import Data.Morphosyntax.Tagset (tagSim)

type Logger = Writer [String]

-- | Synchronize two data sets, taking disamb tags from the first one
-- and the rest form the second one.
sync :: [SentMlt] -> [SentMlt] -> Logger [SentMlt]
sync xs ys = 
    segment segm . concat <$> mapM (uncurry syncWord) zs
  where
    -- | Sentence-level segmentation.
    segm = map length ys
    zs = align (concat xs) (concat ys)

syncWord :: [WordMlt] -> [WordMlt] -> Logger [WordMlt]
syncWord [x] [y] = do
    mlt <- censor (map info) (mergeMulti (choice x) (interps $ word y))
    return [(word y, mlt)]
  where
    form = L.unpack . orth . word
    info log = "word: " ++ form x ++ " " ++ log
syncWord xs ys = do
    tell ["multi: " ++ show xs]
    return xs

mergeMulti :: Multi -> [Interp] -> Logger Multi
mergeMulti mlt xs = concat <$> mapM (mergeDisamb xs) mlt

mergeDisamb :: [Interp] -> (Interp, Double) -> Logger [(Interp, Double)]
mergeDisamb xs (x, pr)
    | Just x' <- find (==x) xs = return [(x, pr)]
    | otherwise = do
        tell ["choice not in interp list"]
        let sim = maximum $ map (tagSim (tag x) . tag) xs
            res = [(y, pr') | y <- xs, tagSim (tag x) (tag y) == sim]
            pr' = pr / fromIntegral (length res)
        return res

segment :: [Int] -> [a] -> [[a]]
segment (n:ns) xs = take n xs : segment ns (drop n xs)
segment [] [] = []
