{-# LANGUAGE OverloadedStrings #-}

module Sat where
import Control.Monad.Combinators.Expr
import Data.Fix
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Control.Monad.State.Lazy (MonadState (..), evalState)
import Data.List (intercalate)
import Prelude hiding (lookup)

data SatF v a =  Var v | Not a | And a a | Or a a deriving (Show, Eq, Functor, Traversable, Foldable)
type Sat = Fix (SatF String)

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

pVariable :: Parser Sat
pVariable = wrapFix . Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Sat
pTerm = choice
  [ parens pexpr
  , pVariable
  ]

operatorTable :: [[Operator Parser Sat]]
operatorTable =
  [ [ prefix "!" (wrapFix . Not)
    ]
  , [ binary "*" (\x -> wrapFix . And x)
    ]
  , [ binary "+" (\x -> wrapFix . Or x)
    ]
  ]

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

pexpr = makeExprParser pTerm operatorTable
parser = pexpr <* eof

data Tag a = Index Int a | Name String a deriving (Functor)
instance Show a => Show (Tag a) where
  show (Index a b) = show b ++ show a
  show (Name a b) = show b ++ a


type SatI = Fix (Compose Tag (SatF String))

index :: Sat -> SatI
index sat = evalState (foldFixM folder sat) 0
  where    
    folder expr = get >>= fmap (Fix . Compose) . f expr

    f expr@(Var x) _ = return $ Name x expr
    f expr idx = put (idx + 1) >> return (Index idx expr)

type Variable = Int

astMatch :: a -> a -> Fix (Compose Tag (SatF v)) -> [[Tag a]]
astMatch negated normal ast = [normal <$ x] : inner negated normal ast
  where
    (Compose x) = unwrapFix ast

    inner f t = g . getCompose . fmap unwrapFix . unwrapFix
      where
        inner' s = inner f t (wrapFix s)
        g a@(Index _ (Not s'@(Compose b))) = [f <$ a, f <$ b] : [t <$ a, t <$ b] : inner' s'
        g a@(Index _ (And s'@(Compose b) s''@(Compose c))) = [f <$ a, t <$ b] : [f <$ a, t <$ c] : [t <$ a, f <$ b, f <$ c] : inner' s' ++ inner' s''
        g a@(Index _ (Or s'@(Compose b) s''@(Compose c))) = [t <$ a, f <$ b] : [t <$ a, f <$ c] : [f <$ a, t <$ b, t <$ c] : inner' s' ++ inner' s''
        g _ = []

data Prefix = Inverse | Normal
instance Show Prefix where
  show Inverse = "¬"
  show Normal = ""

newtype CNF a = CNF [[Tag a]]

instance Show a => Show (CNF a) where
  show (CNF l) = intercalate " ∧ " (par . intercalate " ∨ " . fmap show <$> l)
    where
      par x = "( " ++ x ++ " )"

tseitin :: Sat -> CNF Prefix
tseitin = CNF . astMatch Inverse Normal . index
