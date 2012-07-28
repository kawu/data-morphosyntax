{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Text.Morphosyntax.Tag
( parseTag
, showTag
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import Data.Maybe (catMaybes)

import Data.Morphosyntax.Tagset

import Debug.Trace (trace)

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
    parseRule ((_, True):atts) [] = parseRule atts []
    parseRule as [] = error $ "parseRule: unexpected end of input in tag "
        ++ L.unpack inp
    parseRule [] xs = error $ "parseRule: input too long in tag "
        ++ L.unpack inp

showTag :: Tagset -> Tag -> L.Text
showTag tagset tag =
    -- trace (L.unpack result) result
    result
  where
    result =  L.intercalate ":" (pos tag : catMaybes attrVals)
    attrVals = map showAttr $ rule tagset (pos tag)
    showAttr (attr, opt)
        | Just x <- M.lookup attr (atts tag) = Just x
        | opt == True = Nothing
        | otherwise = error $
            "showTag: no value for mandatory attribute " ++ L.unpack attr
