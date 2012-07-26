{-# LANGUAGE RecordWildCards #-}

module Data.Morphosyntax.Tagset
( Tagset (..)
, Attr
, AttrVal
, POS
, Optional
, domain
, rule
, Tag (..)
) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Binary

type Attr       = L.Text
type AttrVal    = L.Text
type POS        = L.Text    -- ^ Part of speech value
type Optional   = Bool

data Tagset = Tagset
    { domains   :: M.Map Attr (S.Set AttrVal)
    , rules     :: M.Map POS  [(Attr, Optional)]
    } deriving (Show)

-- | FIXME: Move to separate module.
instance Binary L.Text where
    put = put . L.encodeUtf8
    get = L.decodeUtf8 <$> get

instance Binary Tagset where
    put ts = put (domains ts)
          >> put (rules ts)
    get = liftM2 Tagset get get

domain :: Tagset -> Attr -> S.Set AttrVal
domain Tagset{..} x =
  case x `M.lookup` domains of
    Just y  -> y
    Nothing -> error $ "domain: unknown attribute " ++ L.unpack x

rule :: Tagset -> POS -> [(Attr, Optional)]
rule Tagset{..} x =
  case x `M.lookup` rules of
    Just y  -> y
    Nothing -> error $ "rule: unknown POS " ++ L.unpack x

data Tag = Tag
    { pos   :: POS
    , atts  :: M.Map Attr AttrVal
    } deriving (Show, Read, Eq, Ord)

instance Binary Tag where
    put Tag{..} = put pos >> put atts
    get = Tag <$> get <*> get
