{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Morphosyntax.Plain
( parsePlain
, parseSent
) where

import Data.Maybe (catMaybes)
import Data.List (groupBy, intercalate)
import qualified Data.Text.Lazy as L

import Data.Morphosyntax.Canonical (Space (..))
import qualified Data.Morphosyntax.Raw as M

type Sent   = [(M.Word, M.Disamb)]

parsePlain :: L.Text -> [Sent]
parsePlain = map parseSent . init . L.splitOn "\n\n"

parseSent :: L.Text -> Sent
parseSent
    = map parseWord
    . groupBy (\_ x -> pred x)
    . L.lines
  where
    pred = ("\t" `L.isPrefixOf`)

parseWord :: [L.Text] -> (M.Word, M.Disamb)
parseWord xs
    | length choice == 1 = (M.Word orth space forms, head choice)
    | otherwise = error $ "parseWord: ambiguous choice in:\n"
        ++ intercalate "\n" (map L.unpack xs)
  where
    (orth, space) = parseHeader (head xs)

    ys = map parseInterp (tail xs)
    forms = if Nothing `elem` ys
        then []
        else map fst $ catMaybes ys

    choice = [y | (y, True) <- catMaybes ys]

parseInterp :: L.Text -> Maybe (M.Interp, Bool)
parseInterp =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, "ign"] = Nothing
    doIt [form, tag] = Just $
        (M.Interp form tag, False)
    doIt [form, tag, "disamb"] = Just $
        (M.Interp form tag, True)
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
