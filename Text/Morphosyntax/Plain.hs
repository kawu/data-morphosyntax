{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Morphosyntax.Plain
( parsePlain
, parseSent
, parsePlainR
, parseSentR
) where

import Data.Maybe (catMaybes)
import Data.List (groupBy, intercalate)
import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Base (Space (..))
import Data.Morphosyntax.Tagset (Tagset)
import qualified Data.Morphosyntax.Raw as Raw
import qualified Data.Morphosyntax.Canonical as Cano

parsePlain :: Tagset -> L.Text -> [Cano.SentMlt]
parsePlain tagset = map (Raw.toCanoSentMlt tagset) . parsePlainR

parseSent :: Tagset -> L.Text -> Cano.SentMlt
parseSent tagset = Raw.toCanoSentMlt tagset . parseSentR

parsePlainR :: L.Text -> [Raw.SentMlt]
parsePlainR = map parseSentR . init . L.splitOn "\n\n"

parseSentR :: L.Text -> Raw.SentMlt
parseSentR
    = map parseWord
    . groupBy (\_ x -> pred x)
    . L.lines
  where
    pred = ("\t" `L.isPrefixOf`)

parseWord :: [L.Text] -> (Raw.Word, Raw.Multi)
parseWord xs =
    (Raw.Word orth space forms, choice)
  where
    (orth, space) = parseHeader (head xs)

    ys = map parseInterp (tail xs)
    forms = if Nothing `elem` ys
        then []
        else map fst $ catMaybes ys

    choice = [(y, prob) | (y, True) <- catMaybes ys]
    prob = 1 / fromIntegral (length choice)

parseInterp :: L.Text -> Maybe (Raw.Interp, Bool)
parseInterp =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, "ign"] = Nothing
    doIt [form, tag] = Just $
        (Raw.Interp form tag, False)
    doIt [form, tag, "disamb"] = Just $
        (Raw.Interp form tag, True)
    doIt xs = error $ "parseInterp: " ++ show xs

parseHeader :: L.Text -> (L.Text, Space)
parseHeader xs =
    let [orth, space] = L.splitOn "\t" xs
    in  (orth, parseSpace space)

parseSpace :: L.Text -> Space
parseSpace "none"    = NoSpace
parseSpace "space"   = Space
parseSpace "newline" = NewLine
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)
