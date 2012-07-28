{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Morphosyntax.Plain
( parsePlain
, parseSent
, parsePlainR
, parseSentR

, showPlain
, showSent
, showWord
) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Maybe (catMaybes)
import Data.List (groupBy, intercalate)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.Set as S

import Data.Morphosyntax.Base (Space (..))
import Data.Morphosyntax.Tagset (Tagset)
import qualified Data.Morphosyntax.Raw as Raw
import qualified Data.Morphosyntax.Canonical as Cano
import Text.Morphosyntax.Tag (showTag)

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

-- | Printing.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

showPlain :: Tagset -> [Cano.SentMlt] -> L.Text
showPlain tagset =
    L.toLazyText . mconcat  . map (\xs -> buildSent tagset xs <> "\n")

showSent :: Tagset -> Cano.SentMlt -> L.Text
showSent tagset = L.toLazyText . buildSent tagset

showWord :: Tagset -> (Cano.Word, Cano.Multi) -> L.Text
showWord tagset = L.toLazyText . buildWord tagset

buildSent :: Tagset -> Cano.SentMlt -> L.Builder
buildSent tagset = mconcat . map (buildWord tagset)

buildWord :: Tagset -> (Cano.Word, Cano.Multi) -> L.Builder
buildWord tagset (word, multi)
    =  L.fromLazyText (Cano.orth word) <> "\t"
    <> buildSpace (Cano.space word) <> "\n"
    <> buildInterps tagset multi interps
  where
    -- | We hadle the special case here, when the set of choices
    -- is not a subset of the set of interpretations. It is
    -- convenient, because after guessing we will have forms with
    -- "None" base form, which will cause the situation described.
    interps = Cano.interps word ++
        [x | (x, _) <- multi, not (x `S.member` interpSet)]
    interpSet = S.fromList (Cano.interps word)

buildInterps :: Tagset -> Cano.Multi -> [Cano.Interp] -> L.Builder
buildInterps tagset multi interps = mconcat
    [ "\t" <> L.fromLazyText (Cano.base x) <>
      "\t" <> L.fromLazyText (showTag tagset $ Cano.tag x) <>
      if x `S.member` choice
        then "\tdisamb\n"
        else "\n"
    | x <- interps ]
  where
    choice = S.fromList $ map fst multi

buildSpace :: Cano.Space -> L.Builder
buildSpace NoSpace  = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"
