module Main where

import Control.Monad (when)
import Control.Monad.State (State(..), get, put, execState)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Vector as V

data Grid = Grid { gData :: V.Vector Int, gWidth :: Int } deriving (Show, Eq)
data Point = Point { pX :: Int, pY :: Int } deriving (Show, Eq)
data Line = Line { lStart :: Point, lEnd :: Point } deriving (Show, Eq)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn p = splitOn' [[]]
  where splitOn' acc []                             = reverse $ reverse <$> acc
        splitOn' acc (x:xs) | p `isPrefixOf` (x:xs) = splitOn' ([]:acc) (drop (length p) (x:xs))
                            | otherwise             = let (xs':xss') = acc
                                                      in splitOn' ((x:xs'):xss') xs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

count :: (a -> Bool) -> V.Vector a -> Int
count f v = count' 0 0
  where count' acc n | n == V.length v = acc
                     | f (v V.! n)     = count' (acc + 1) (n + 1)
                     | otherwise       = count' acc (n + 1)

maxSize :: Line -> Int
maxSize (Line (Point x1 y1) (Point x2 y2)) = max (max x1 y1) (max x2 y2)

pretty :: Grid -> String
pretty (Grid d w) = unlines $ show' 0
  where show' n | n <= (V.length d - w) = V.foldr (<>) "" (V.map showValue (V.slice n w d)) : show' (n + w)
                | otherwise             = []
        showValue 0 = "."
        showValue n = show n

parsePoint :: String -> Point
parsePoint raw = Point x y
  where [x, y] = read . trim <$> splitOn "," raw

parseLine :: String -> Line
parseLine raw = Line start end
  where [start, end] = parsePoint . trim <$> splitOn "->" raw

insertLineIf :: (Line -> Bool) -> Line -> State Grid ()
insertLineIf f l@(Line (Point x1 y1) (Point x2 y2)) = do
  Grid d w <- get
  when (f l) $ do
    let updates = flip concatMap [(min x1 x2)..(max x1 x2)] $ \x ->
                    flip map [(min y1 y2)..(max y1 y2)] $ \y ->
                      let i = y * w + x
                      in (i, (d V.! i) + 1)
    put (Grid (d V.// updates) w)

insertLine :: Line -> State Grid ()
insertLine = insertLineIf (const True)

solution :: Grid -> Int
solution = count (> 1) . gData

main :: IO ()
main = do
  ls <- (parseLine <$>) . lines <$> readFile "resources/input.txt"
  let w  = maximum (maxSize <$> ls) + 1
      g0 = Grid (V.replicate (w * w) 0) w
      g1 = execState (mapM (insertLineIf (\(Line (Point x1 y1) (Point x2 y2)) -> (x1 == x2) || (y1 == y2))) ls) g0
      g2 = execState (mapM insertLine ls) g0
  putStrLn $ "Part 1: " ++ show (solution g1)
  putStrLn $ "Part 2: " ++ show (solution g2)
