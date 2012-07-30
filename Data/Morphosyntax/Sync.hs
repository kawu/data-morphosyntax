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

import Debug.Trace (trace)

-- | Synchronize two data sets, taking disamb tags from the first one
-- and the rest form the second one.
sync :: [WordMlt] -> [WordMlt] -> [WordMlt]
sync xs ys = concatMap (uncurry syncWord) (align xs ys)

syncWord :: [WordMlt] -> [WordMlt] -> [WordMlt]
syncWord [x] [y] =
    let mlt = mergeMulti (choice x) (interps $ word y)
    in  [(word y, mlt)]
  where
syncWord xs ys = trace "syncWord xs" xs

mergeMulti :: Multi -> [Interp] -> Multi
mergeMulti mlt xs = concatMap (mergeDisamb xs) mlt

mergeDisamb :: [Interp] -> (Interp, Double) -> [(Interp, Double)]
mergeDisamb xs (x, pr)
    | Just x' <- find (==x) xs = [(x, pr)]
    | otherwise =
        let sim = maximum $ map (tagSim (tag x) . tag) xs
            res = [(y, pr') | y <- xs, tagSim (tag x) (tag y) == sim]
            pr' = pr / fromIntegral (length res)
        in  res
