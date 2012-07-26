{-# LANGUAGE RecordWildCards #-}

module Data.Morphosyntax.Base
( Interp (..)
, Space (..)
, Disamb
, Multi
) where

import qualified Data.Text.Lazy as L
import Control.Applicative ((<$>), (<*>))
import Data.Binary

import Data.Morphosyntax.Tagset (Tag)

data Space
    = NoSpace
    | Space
    | NewLine
    deriving (Show, Read, Eq, Ord)

data Interp = Interp
    { base :: L.Text
    , tag  :: Tag }
    deriving (Show, Read, Eq, Ord)

type Disamb = Interp
type Multi  = [(Interp, Double)]    -- ^ Interpretations with probabilities

instance Binary Space where
    put = put . code
      where
        code :: Space -> Int
        code NoSpace = 0
        code Space   = 1
        code NewLine = 2
    get = decode <$> get
      where
        decode :: Int -> Space
        decode 0 = NoSpace
        decode 1 = Space
        decode 2 = NewLine

instance Binary Interp where
    put Interp{..} = put base >> put tag
    get = Interp <$> get <*> get
