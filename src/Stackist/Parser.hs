
{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}
module Stackist.Parser
 (parseExpr, Expr(..), parseJoy)
where

import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data Expr = Numeric Integer
          | Literal String
          | JString String
          | Quote [Expr]
          | Boolean Bool
            deriving (Read, Eq, Show)

-- instance Show Literal where
--    show (Literal x) = show x

parseString :: Parser Expr
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ JString x

parseAtom :: Parser Expr
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Boolean True
                          "#f" -> Boolean False
                          _ -> Literal atom

parseNumber :: Parser Expr
parseNumber = fmap (Numeric . read) $ many1 digit

parseList :: Parser Expr
parseList = fmap Quote $ sepBy parseExpr spaces

parseQuoted :: Parser Expr
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ Quote [Literal "quote", x]

parseExpr :: Parser Expr
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do _ <- char '['
               x <- (try parseList)
               _ <- char ']'
               return x


-- | parseJoy "[1 2 +]"
--
-- >> parseJoy "[1 2 +]"
-- [Quote [Numeric 1, Numeric 2, Literal "+"]]
parseJoy :: String -> Either ParseError Expr
parseJoy input = parse parseExpr "(unknown)" input
