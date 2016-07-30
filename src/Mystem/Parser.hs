{-# LANGUAGE OverloadedStrings #-}

module Mystem.Parser where

  import           Control.Applicative
  import           Control.Monad
  import           Data.Attoparsec.Text ((<?>))
  import qualified Data.Attoparsec.Text as P
  import           Data.Default
  import qualified Data.Text            as T

  import           Mystem.Types

  parserMystems :: P.Parser [MSRes]
  parserMystems = P.sepBy parserMystem P.endOfLine

  parserMystem :: P.Parser MSRes
  parserMystem = do
    P.skipMany P.endOfLine
    sw <- getSWord
    P.skipMany1 (P.char '{') <?> "open sym"
    rws <- P.sepBy1 getRWord (P.char '|') <?> "words"
    P.skipMany1 (P.char '}') <?> "closing sym"
    return $ MSRes sw rws

  getRWord :: P.Parser RWord
  getRWord = do
    rw <- getResultWord
    P.skipMany $ P.char '?'
    gs <- optional $ P.char '='
    flip (maybe (return $ RWord rw (Pos Nothing) def [])) gs $ \_ -> do
      rPos <- getPos
      rGramm <- optional getGramm
      void (P.char '=' <?> "gramm end")
      cs <- P.char '(' *> P.sepBy1 getGramm (P.char '|') <* P.char ')'
              <|> flip (:) [] <$> getGramm
                <|> pure []
      return $ RWord rw rPos rGramm cs

  getGramm :: P.Parser Grams
  getGramm = do
    l <- gsList
    case l of
      [] -> fail "no gramms"
      xs -> return $ fillGrams def xs

  getPos :: P.Parser Pos
  getPos = do
    p <- P.many1 msWChar <?> "pos"
    P.skipMany $ P.char ','
    return $ Pos . readG $ p

  getResultWord :: P.Parser WordT
  getResultWord = T.pack <$> (P.many1 msWChar <?> "rword")

  getSWord :: P.Parser SWord
  getSWord = T.pack <$> (P.many1 msWChar <?> "sword")

  gsList :: P.Parser [String]
  gsList = P.sepBy gsWString (P.char ',')

  gsWString :: P.Parser String
  gsWString = P.many1 (P.letter <|> P.digit)

  gsWChar :: P.Parser Char
  gsWChar = P.letter <|> P.digit

  msWChar :: P.Parser Char
  msWChar = P.letter <|> P.digit <|> P.char '-'
