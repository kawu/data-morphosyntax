module Text.Morphosyntax.Tagset
( parseTagset
) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Text.Parsec
import Data.Char (isSpace)

import qualified Data.Text.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Morphosyntax.Tagset

-- type Parser a = Parsec String () a
type Parser = Parsec String ()

tagsetFile :: Parser Tagset
tagsetFile = spaces *> (Tagset <$> attrSec <*> ruleSec)

attrSec :: Parser (M.Map Attr (S.Set AttrVal))
attrSec = do
    secName "ATTR" *> spaces
    defs <- attrLine `endBy` spaces
    return $ M.fromList defs

attrLine :: Parser (Attr, S.Set AttrVal)
attrLine = do
    attr <- ident
    spaces *> char '=' *> lineSpaces
    values <- map L.pack <$> ident `endBy` lineSpaces
    return (L.pack attr, S.fromList values)

ruleSec :: Parser (M.Map POS [(Attr, Optional)])
ruleSec = do
    secName "RULE" *> spaces
    M.fromList <$> ruleLine `endBy` spaces

ruleLine :: Parser (POS, [(Attr, Optional)])
ruleLine = do
    pos <- ident
    lineSpaces *> char '=' *> lineSpaces
    actionAtts <- attrName `endBy` lineSpaces
    return $ (L.pack pos, actionAtts)

attrName :: Parser (Attr, Optional)
attrName = optionalAttrName <|> plainAttrName <?> "attribute name"
optionalAttrName = do
    char '['
    name <- ident
    char ']'
    return (L.pack name, True)
plainAttrName = do
    name <- ident
    return $ (L.pack name, False)

lineSpace = satisfy $ \c -> (isSpace c) && (not $ c == '\n')
lineSpaces = many lineSpace

ident = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "."

secName name = char '[' *> string name *> char ']'

parseTagset :: String -> String -> Tagset
parseTagset src contents = do
    case parse tagsetFile src filtered of
        Left e  -> error $ "Error parsing input:\n" ++ show e
        Right r -> r
  where
     filtered = unlines $ map (removeComment '#') $ lines contents

removeComment :: Char -> String -> String
removeComment commChar s = case findComment s of
    Just i -> fst $ splitAt i s
    Nothing -> s
    where
        findComment s = doFind s 0 False
        doFind (x:xs) acc inQuot
            | x == commChar && not inQuot = Just acc
            | x == '"' = doFind xs (acc + 1) (not inQuot)
            | otherwise =  doFind xs (acc + 1) inQuot
        doFind [] _ _ = Nothing
