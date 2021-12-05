module Main where

import Control.Monad.State ( State(..), get, put )
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Vector as V

data Grid = Grid { gData :: V.Vector Int, gWidth :: Int }
data Point = Point { pX :: Int, pY :: Int }
data Line = Line { lStart :: Point, lEnd :: Point }

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn p = splitOn' [[]]
  where splitOn' acc []                             = reverse $ reverse <$> acc
        splitOn' acc (x:xs) | p `isPrefixOf` (x:xs) = splitOn' ([]:acc) (drop (length p) (x:xs))
                            | otherwise             = let (xs':xss') = acc
                                                      in splitOn' ((x:xs'):xss') xs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parsePoint :: String -> Point
parsePoint raw = Point x y
  where [x, y] = read . trim <$> splitOn "," raw

parseLine :: String -> Line
parseLine raw = Line start end
  where [start, end] = parsePoint . trim <$> splitOn "->" raw

insertLine :: Line -> State Grid ()
insertLine (Line (Point x1 y1) (Point x2 y2)) = do
  Grid d w <- get
  let updates = flip concatMap [(min x1 x2)..(max x1 x2)] $ \x ->
                  flip map [(min y1 y2)..(max y1 y2)] $ \y ->
                    let i = y * w + x
                    in (i, (d V.! i) + 1)
  put (Grid (d V.// updates) w)

main :: IO ()
main = putStrLn "Hello!"
