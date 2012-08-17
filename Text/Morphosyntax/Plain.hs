{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Morphosyntax.Plain
( parsePlain
, parseSent

, showPlain
, showSent
, showWord
) where

import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.Set as S

import qualified Data.Morphosyntax.Canonical as Cano
import Data.Morphosyntax.Canonical (Space(..))

type TagL = L.Text
type Tag  = T.Text

parsePlain :: L.Text -> [[Cano.Disamb TagL]]
parsePlain = map parseSent . init . L.splitOn "\n\n"

parseSent :: L.Text -> [Cano.Disamb TagL]
parseSent
    = map parseWord
    . groupBy (\_ x -> pred x)
    . L.lines
  where
    pred = ("\t" `L.isPrefixOf`)

parseWord :: [L.Text] -> Cano.Disamb TagL
parseWord xs = Cano.Disamb
    (Cano.Word orth space known forms) choice
  where
    (orth, space) = parseHeader (head xs)

    ys = map parseInterp (tail xs)
    known = not (Nothing `elem` ys)
    forms = map fst (catMaybes ys)

    choice = [(y, prob) | (y, True) <- catMaybes ys]
    prob = 1 / fromIntegral (length choice)

parseInterp :: L.Text -> Maybe (Cano.Interp TagL, Bool)
parseInterp =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, "ign"] = Nothing
    doIt [form, tag] = Just $
        (mkInterp form tag, False)
    doIt [form, tag, "disamb"] = Just $
        (mkInterp form tag, True)
    doIt xs = error $ "parseInterp: " ++ show xs
    mkInterp form tag = Cano.Interp (L.toStrict form) tag

parseHeader :: L.Text -> (T.Text, Space)
parseHeader xs =
    let [orth, space] = L.splitOn "\t" xs
    in  (L.toStrict orth, parseSpace space)

parseSpace :: L.Text -> Space
parseSpace "none"    = NoSpace
parseSpace "space"   = Space
parseSpace "newline" = NewLine
parseSpace "newlines" = NewLine -- ^ TODO: Remove this temporary fix
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)

-- | Printing.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

showPlain :: [[Cano.Disamb Tag]] -> L.Text
showPlain =
    L.toLazyText . mconcat  . map (\xs -> buildSent xs <> "\n")

showSent :: [Cano.Disamb Tag] -> L.Text
showSent = L.toLazyText . buildSent

showWord :: Cano.Disamb Tag -> L.Text
showWord = L.toLazyText . buildWord

buildSent :: [Cano.Disamb Tag] -> L.Builder
buildSent = mconcat . map buildWord

buildWord :: Cano.Disamb Tag -> L.Builder
buildWord dmb
    =  L.fromText (Cano.orth word) <> "\t"
    <> buildSpace (Cano.space word) <> "\n"
    <> buildKnown (Cano.known word)
    <> buildInterps dmb interps
  where
    -- | We hadle the special case here, when the set of choices
    -- is not a subset of the set of interpretations. It is
    -- convenient, because after guessing we will have forms with
    -- "None" base form, which will cause the situation described.
    interps = Cano.interps word ++
        [ x | (x, _) <- choice
            , not (x `S.member` interpSet) ]
    interpSet = S.fromList (Cano.interps word)
    word = Cano.word dmb
    choice = Cano.choice dmb

buildInterps :: Cano.Disamb Tag -> [Cano.Interp Tag] -> L.Builder
buildInterps dmb interps = mconcat
    [ "\t" <> L.fromText (Cano.base x) <>
      "\t" <> L.fromText (Cano.tag x) <>
      if x `S.member` choice
        then "\tdisamb\n"
        else "\n"
    | x <- interps ]
  where
    choice = S.fromList $ map fst $ Cano.choice dmb

buildSpace :: Cano.Space -> L.Builder
buildSpace NoSpace  = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"

buildKnown :: Bool -> L.Builder
buildKnown True     = ""
buildKnown False    = "\tNone\tign\n"
