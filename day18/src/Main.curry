module Main where

data Snail = Pair Int Int | Regular Int deriving (Eq, Show)

pretty :: Snail -> String
pretty (Pair x y) = "[" ++ pretty x ++ "," ++ pretty y ++ "]"
pretty (Regular x) = show x

snail :: Parser Snail
snail = pair <|> regular
  where pair = char '[' *> 

main :: IO ()
main = putStrLn "This is my project!"
