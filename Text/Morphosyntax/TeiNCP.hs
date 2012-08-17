{-# LANGUAGE OverloadedStrings #-}

module Text.Morphosyntax.TeiNCP
( ID
, Para (..)
, Sent (..)
, Seg (..)
, Lex (..)
, parseMorph
, readMorph
, canoWord
) where

import System.FilePath (takeDirectory, takeBaseName)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (groupBy, find)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified Data.Set as S

import Text.XML.PolySoup
import qualified Data.Morphosyntax.Canonical as Cano

-- | TEI format instance adapted for the National Corpus of Polish (NCP). 
-- TODO: Move data defs to separate module.

type ID = L.Text

-- | Paragraph.
data Para = Para
    { paraID    :: ID
    , sentences :: [Sent] }
    deriving (Show)

-- | Sentence.
data Sent = Sent
    { sentID    :: ID
    , segments  :: [Seg] }
    deriving (Show)

-- | Segment.
data Seg = Seg
    { segID     :: ID 
    , orth      :: L.Text
    , nps       :: Bool
    , lexs      :: [Lex]
    , choice    :: (ID, L.Text) }
    deriving (Show)

-- | Lexciacal entry -- possible interpretation.
data Lex = Lex
    { lexID     :: ID
    , base      :: L.Text
    , ctag      :: L.Text
    , msds      :: [(ID, L.Text)] }
    deriving (Show)

-- | Conversion between native TEI NKJP data format and the canonical form.
canoWord :: Seg -> Cano.Disamb T.Text
canoWord seg =
    Cano.Disamb word disamb
  where
    word = Cano.Word
        { Cano.orth = L.toStrict (orth seg)
        -- | New lines are not represented in NCP.
        , Cano.space = case nps seg of
            True  -> Cano.NoSpace
            False -> Cano.Space
        , Cano.known = not ("ign" `S.member` ctags)
        , Cano.interps =
            [ mkInterp lex msd
            | lex <- lexs seg
            , (_, msd) <- msds lex ] }

    choiceID = fst (choice seg)
    disamb = snd . fromJust . find ((==choiceID).fst) $
        [ (msdID, [(mkInterp lex msd, 1.0)])
        | lex <- lexs seg
        , (msdID, msd) <- msds lex ]

    mkInterp lex msd = Cano.Interp
        (L.toStrict $ base lex)
        (L.toStrict $ mkTag (ctag lex) msd)

    mkTag ctag msd = if L.null msd
        then ctag
        else ctag `L.append` ":" `L.append` msd

    ctags = S.fromList . map ctag $ lexs seg

-- | TEI NKJP ann_morphosyntax parser.
type P a = XmlParser L.Text a

morphP :: P [Para]
morphP = true //> paraP

paraP :: P Para
paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

sentP :: P Sent
sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> segP)

segP :: P Seg
segP = (tag "seg" *> getAttr "xml:id") `join` smP

smP :: ID -> P Seg
smP segID = (tag "fs" *> hasAttr "type" "morph") `joinR` do
    orth <- fStrP "orth"
    nps  <- optional $ cut $ hasAttr "name" "nps"
    lexs <- hasAttr "name" "interps" /> lexP
    choice <- choiceP
    return (Seg segID orth (isJust nps) lexs choice)

lexP :: P Lex
lexP = (hasAttr "type" "lex" *> getAttr "xml:id") `join` \lexID -> do
    base <- fStrP "base"
    ctag <- fSymP "ctag"
    msds <- hasAttr "name" "msd" //> cut
        ((,) <$> (tag "symbol" *> getAttr "xml:id") <*> getAttr "value")
    return (Lex lexID base ctag msds)

choiceP :: P (ID, L.Text)
choiceP = hasAttr "name" "disamb" `joinR` ( tag "fs" `joinR` do
    ptr <- L.tail <$> cut (getAttr "fVal")
    interp <- fStrP "interpretation"
    return (ptr, interp) )

fStrP :: L.Text -> P L.Text
fStrP x =
    let checkName = tag "f" *> hasAttr "name" x
        -- | Body sometimes is empty.
        safeHead [] = ""
        safeHead xs = head xs
    in  safeHead <$> (checkName #> tag "string" /> text)

fSymP :: L.Text -> P L.Text
fSymP x =
    let checkName = tag "f" *> hasAttr "name" x
        p = cut (tag "symbol" *> getAttr "value")
    in  head <$> (checkName /> p)

parseMorph :: L.Text -> [Para]
parseMorph = parseXML morphP

-- | NKJP .tar.gz handling.  TODO: Move NCP parsing utilities
-- to another, common module.  It should also allow parsing
-- plain directories.

-- | Parse NCP .tar.gz corpus.
readMorph :: FilePath -> IO [(FilePath, [Para])]
readMorph tarPath = do
    map parseEntry . withBase "ann_morphosyntax" <$> readTar tarPath

readTar :: FilePath -> IO [Tar.Entry]
readTar tar
    =  Tar.foldEntries (:) [] error
    .  Tar.read . GZip.decompress
   <$> BS.readFile tar

parseEntry :: Tar.Entry -> (FilePath, [Para])
parseEntry entry =
    (Tar.entryPath entry, parseMorph content)
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = L.decodeUtf8 binary

-- filterMorph :: [Tar.Entry] -> [Tar.Entry]
-- filterMorph
--     = catMaybes 
--     . map (getEntry "ann_morphosyntax")
--     . groupBy ((==) `on` takeDirectory . Tar.entryPath)
--   where
--     getEntry base = find ((==base) . takeBaseName . Tar.entryPath)

withBase :: String -> [Tar.Entry] -> [Tar.Entry]
withBase base = filter ((==base) . takeBaseName . Tar.entryPath)
