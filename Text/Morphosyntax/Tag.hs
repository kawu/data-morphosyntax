{-# LANGUAGE OverloadedStrings #-}

module Text.Morphosyntax.Tag
( parseTag
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Tagset

parseTag :: Tagset -> L.Text -> Tag
parseTag tagset inp =
    Tag pos $ M.fromList $ parseRule (rule tagset pos) atts
  where
    (pos:atts) = L.split (==':') inp
    parseRule ((attr, opt):atts) (x:xs)
        | x `S.member` domain tagset attr
            = (attr, x) : parseRule atts xs
        | opt == True   = parseRule atts (x:xs)
        | otherwise     = error $ "parseRule:"
            ++ " no value for " ++ L.unpack attr
            ++ " attribute in tag " ++ L.unpack inp
    parseRule [] [] = []
    parseRule as [] = error $ "parseRule: unexpected end of input in tag "
        ++ L.unpack inp
    parseRule [] xs = error $ "parseRule: input too long in tag "
        ++ L.unpack inp