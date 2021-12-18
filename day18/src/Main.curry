module Main where

import Debug.Trace
import Data.Maybe (fromMaybe)
import Parsing

data Snail = Pair Snail Snail | Regular Int deriving (Eq, Show)

-- Parsing/prettyprinting

pretty :: Snail -> String
pretty (Pair x y) = "[" ++ pretty x ++ "," ++ pretty y ++ "]"
pretty (Regular x) = show x

snail :: Parser Snail
snail = pair <|> regular
  where
    pair = do char '['
              x <- snail
              char ','
              y <- snail
              char ']'
              return $ Pair x y
    regular = Regular <$> nat

-- Transformations

modifyLeftmost :: (Int -> Int) -> Snail -> Snail
modifyLeftmost f (Regular x) = Regular $ f x
modifyLeftmost f (Pair x y) = Pair (modifyLeftmost f x) y

modifyRightmost :: (Int -> Int) -> Snail -> Snail
modifyRightmost f (Regular x) = Regular $ f x
modifyRightmost f (Pair x y) = Pair x (modifyRightmost f y)

-- Reductions

explode :: Snail -> Maybe Snail
explode s = let (_, s', _, didExplode) = explode' 0 s
            in if didExplode then Just s' else Nothing
  where
    explode' :: Int -> Snail -> (Maybe Int, Snail, Maybe Int, Bool)
    explode' d (Pair x y) | d >= 4    = case (x, y) of
                                          (Regular x', Regular y') -> (Just x', Regular 0, Just y', True)
                                          _                        -> error $ "Found invalid exploding pair: " ++ pretty (Pair x y)
                          | otherwise = let (lx, x', rx, explodedX) = explode' (d + 1) x
                                            (ly, y', ry, explodedY) = explode' (d + 1) y
                                        in if explodedX then
                                            (lx, Pair x' (modifyLeftmost (explosion rx) y), Nothing, True)
                                           else if explodedY then
                                            (Nothing, Pair (modifyRightmost (explosion ly) x) y', ry, True)
                                           else
                                            (Nothing, Pair x y, Nothing, False)
    explode' _ (Regular x)            = (Nothing, Regular x, Nothing, False)

    explosion :: Maybe Int -> Int -> Int
    explosion (Just x) = (+x)
    explosion Nothing  = id

split :: Snail -> Maybe Snail
split s = let (s', didSplit) = split' s
          in if didSplit then Just s' else Nothing
  where
    split' :: Snail -> (Snail, Bool)
    split' (Regular x) | x >= 10   = let xh = x `div` 2
                                         xm = x `mod` 2
                                     in (Pair (Regular xh) (Regular $ xh + xm), True)
                       | otherwise = (Regular x, False)
    split' (Pair x y) = let (x', splitX) = split' x
                            (y', splitY) = split' y
                        in if splitX then (Pair x' y, True) else if splitY then (Pair x y', True) else (Pair x y, False)

reduce :: Snail -> Snail
reduce s = trace ("Reducing " ++ pretty s) $ fromMaybe s $ reduce <$> (explode s <|> split s)

-- Main

main :: IO ()
main = do
  input <- parse snail <$> readFile "resources/simple-demo.txt"
  putStrLn $ show input
