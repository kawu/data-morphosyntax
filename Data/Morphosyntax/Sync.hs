{-# LANGUAGE PatternGuards #-}

module Data.Morphosyntax.Sync
( sync
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import Control.Applicative ((<$>))
import Control.Monad.Writer
import Data.List (find)

import Data.Morphosyntax.Base
import Data.Morphosyntax.Canonical
import Data.Morphosyntax.Compare (align)
import Data.Morphosyntax.Tagset (Tagset, tagSim, expand)

-- | Synchronize two data sets, taking disamb tags from the first one
-- and the rest form the second one.
sync :: Tagset -> [WordMlt] -> [WordMlt] -> [WordMlt]
sync tagset xs ys = concatMap (uncurry (syncWord tagset)) (align xs ys)

syncWord :: Tagset -> [WordMlt] -> [WordMlt] -> [WordMlt]

syncWord tagset [v] [w] =
    [(word w, mlt)]
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
