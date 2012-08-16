{-# LANGUAGE PatternGuards #-}

module Data.Morphosyntax.Sync
( sync
) where

import qualified Data.Set as S
import Data.List (find)

import Data.Morphosyntax.Canonical
import Data.Morphosyntax.Compare (align)
import Data.Morphosyntax.Tagset (Tagset, Tag, tagSim, expand)

type Dmb = Disamb Tag

-- | Synchronize two data sets, taking disamb tags from the first one
-- and the rest form the second one.
sync :: Tagset -> [Dmb] -> [Dmb] -> [Dmb]
sync tagset xs ys = concatMap (uncurry (syncWord tagset)) (align xs ys)

syncWord :: Tagset -> [Dmb] -> [Dmb] -> [Dmb]

syncWord tagset [v] [w] =
    [Disamb (word w) mlt]
  where
    mlt = mergeMulti (interps $ word w) (choice v)
    mergeMulti xs = concatMap (mergeDisamb xs)
    mergeDisamb xs (x, pr)
        | Just x' <- find ( ==x) xs = [(x', pr)]    -- ^ Exact match
        | Just x' <- find (~==x) xs = [(x', pr)]    -- ^ Expanded tag match
        | otherwise                 = [(x , pr)]    -- ^ Controversial
      where
        x ~== y = S.size (label x `S.intersection` label y) > 0
        label   = S.fromList . expand tagset . tag

syncWord tagset xs ys = xs
