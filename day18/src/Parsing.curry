module Parsing where

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

char :: Char -> Parser Char
char c = Parser $ \is -> case is of
  (i:is') | i == c -> Just (c, is')
  _                -> Nothing

parse :: Parser a -> String -> a
parse (Parser p) is = case p is of
  Just (x, _) -> x
  Nothing     -> error $ "Could not parse " ++ is
