module Main where

import Parsing

data Snail = Pair Snail Snail | Regular Int deriving (Eq, Show)

pretty :: Snail -> String
pretty (Pair x y) = "[" ++ pretty x ++ "," ++ pretty y ++ "]"
pretty (Regular x) = show x

snail :: Parser Snail
snail = pair <|> regular
  where pair = do char '['
                  x <- snail
                  char ','
                  y <- snail
                  char ']'
                  return $ Pair x y
        regular = Regular <$> nat

main :: IO ()
main = putStrLn "This is my project!"
