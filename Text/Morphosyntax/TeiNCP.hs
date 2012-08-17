{-# LANGUAGE OverloadedStrings #-}

module Text.Morphosyntax.TeiNCP
( collect
) where

import System.FilePath (takeDirectory, takeBaseName)
import Data.Maybe (catMaybes)
import Data.List (groupBy, find)
import Data.Function (on)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup

-- | TEI format instance adapted for the National Corpus of Polish (NCP). 

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
    , lexs      :: [Lex]
    , choice    :: L.Text }
    deriving (Show)

-- | Lexciacal entry -- possible interpretation.
data Lex = Lex
    { lexID     :: ID
    , base      :: L.Text
    , ctag      :: L.Text
    , msds      :: [MSD] }
    deriving (Show)

-- | Morphosyntactic description.
data MSD = MSD
    { msdID     :: ID
    , msd       :: L.Text }
    deriving (Show)

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
    lexs <- hasAttr "name" "interps" /> lexP
    choice <- head <$> (hasAttr "name" "disamb" //> tag "string" #> text)
    return (Seg segID orth lexs choice)

lexP :: P Lex
lexP = (hasAttr "type" "lex" *> getAttr "xml:id") `join` \lexID -> do
    base <- fStrP "base"
    ctag <- fSymP "ctag"
    msds <- hasAttr "name" "msd" //> cut
        (MSD <$> (tag "symbol" *> getAttr "xml:id") <*> getAttr "value")
    return (Lex lexID base ctag msds)

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

parseMorph :: Tar.Entry -> (FilePath, [Para])
parseMorph entry =
    (Tar.entryPath entry, parseXML morphP content)
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    -- content = BS.toString binary
    content = L.decodeUtf8 binary

-- | NKJP .tar.gz handling.
ignoreFail :: Tar.Entries -> [Tar.Entry]
ignoreFail Tar.Done         = []
ignoreFail (Tar.Next x xs)  = x : ignoreFail xs
ignoreFail (Tar.Fail x)     = error x

entries :: FilePath -> IO [Tar.Entry]
entries tar = ignoreFail . Tar.read . GZip.decompress <$> BS.readFile tar

filterMorph :: [Tar.Entry] -> [Tar.Entry]
filterMorph
    = catMaybes 
    . map (getEntry "ann_morphosyntax")
    . groupBy ((==) `on` takeDirectory . Tar.entryPath)
  where
    getEntry base = find ((==base) . takeBaseName . Tar.entryPath)

collect :: FilePath -> IO [(FilePath, [Para])]
collect tarPath = do
    map parseMorph . filterMorph <$> entries tarPath
