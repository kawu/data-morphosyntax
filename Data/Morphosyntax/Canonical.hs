{-# LANGUAGE DeriveFunctor #-}

module Data.Morphosyntax.Canonical
( Word (..)
, Space (..)
, Interp (..)
, Disamb (..)
, disamb
) where

import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>))
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | TODO: Perhaps better representation would divide
-- interpretations between separate lexemes?

data Space
    = NoSpace
    | Space
    | NewLine
    deriving (Show, Read, Eq, Ord)

data Word t = Word
    -- | Orthographic form.
    { orth    :: T.Text
    -- | Space before the word.
    , space   :: Space
    -- | Is this a known word?
    , known   :: Bool
    -- | List of word interpretations.
    , interps :: [Interp t] }
    deriving (Show, Read, Eq, Ord, Functor)

data Interp t = Interp
    { base :: T.Text
    , tag  :: t }
    deriving (Show, Read, Eq, Ord, Functor)

-- | Disambiguated word.
data Disamb t = Disamb
    { word   :: Word t
    -- | Single or multiple interpretations.
    , choice :: [(Interp t, Double)] }
    deriving (Show, Read, Eq, Ord, Functor)

-- | Retrieve the most probable interpretation.
disamb :: Disamb t -> Interp t
disamb (Disamb _ []) = error "disamb: null choice" 
disamb (Disamb _ xs) = fst $ maximumBy (comparing snd) xs
