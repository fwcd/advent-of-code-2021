module Parsing where

import Data.Maybe (fromMaybe)

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
  fmap f px = return f <*> px

instance Applicative Parser where
  pure = return
  pf <*> px = pf >>= \f -> px >>= \x -> return $ f x

instance Monad Parser where
  return x = Parser $ \is -> Just (x, is)
  Parser p >>= f = Parser $ \is -> case p is of
    Just (x, is') -> let Parser p' = f x in p' is'
    Nothing       -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \is -> case is of
  (i:is') | f i -> Just (i, is')
  _             -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \is -> Just $ case p is of
  Just (x, is') -> (Just x, is')
  Nothing       -> (Nothing, is)

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  optXs <- optional $ many1 p
  let xs = fromMaybe [] optXs
  return (x:xs)

nat :: Parser Int
nat = read <$> many1 digit

parse :: Parser a -> String -> a
parse (Parser p) is = case p is of
  Just (x, _) -> x
  Nothing     -> error $ "Could not parse " ++ is
