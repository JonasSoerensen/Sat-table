{-# LANGUAGE OverloadedStrings #-}

module Sat where
import Control.Monad.Combinators.Expr
import Data.Fix
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)

data Sat v a =  Var v | Not a | And a a | Or a a deriving (Show, Eq, Functor)
type Sat' = Fix (Sat String)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVariable :: Parser Sat'
pVariable = wrapFix . Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Sat'
pTerm = choice
  [ parens pTerm
  , pVariable
  ]

operatorTable :: [[Operator Parser Sat']]
operatorTable =
  [ [ prefix "-" (wrapFix . Not)
    ]
  , [ binary "+" (\x -> wrapFix . And x)
    ]
  , [ binary "*" (\x -> wrapFix . Or x)
    ]
  ]

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

parser = makeExprParser pTerm operatorTable <* eof