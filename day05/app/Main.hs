module Main where

import Control.Monad (when, forM_)
import Control.Monad.ST
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data Grid v = Grid { gData :: v Int, gWidth :: Int }
data Point = Point { pX :: Int, pY :: Int } deriving (Show, Eq)
data Line = Line { lStart :: Point, lEnd :: Point } deriving (Show, Eq)

type STGrid s = Grid (VM.STVector s)
type VGrid = Grid V.Vector

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

pretty :: VGrid -> String
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

points :: Line -> [Point]
points (Line start@(Point x1 y1) end@(Point x2 y2)) = points' start
  where dx = signum (x2 - x1)
        dy = signum (y2 - y1)
        points' p@(Point x y) | p == end  = [end]
                              | otherwise = p : points' (Point (x + dx) (y + dy))

insertLineIf :: (Line -> Bool) -> STGrid s -> Line -> ST s ()
insertLineIf f (Grid d w) l = when (f l) $ do
  forM_ (points l) $ \(Point x y) ->
    let i = y * w + x
    in VM.modify d (+1) i

paintGrid :: (Line -> Bool) -> [Line] -> VGrid
paintGrid f ls = runST $ do
  let w    = maximum (maxSize <$> ls) + 1
      size = w * w
  d <- VM.replicate size 0
  forM_ ls $ insertLineIf f $ Grid d w
  d' <- V.freeze d
  return $ Grid d' w

solution :: VGrid -> Int
solution = count (> 1) . gData

main :: IO ()
main = do
  ls <- (parseLine <$>) . lines <$> readFile "resources/input.txt"
  let w  = maximum (maxSize <$> ls) + 1
      g0 = Grid (V.replicate (w * w) 0) w
      g1 = paintGrid (\(Line (Point x1 y1) (Point x2 y2)) -> (x1 == x2) || (y1 == y2)) ls
      g2 = paintGrid (const True) ls
  putStrLn $ "Part 1: " ++ show (solution g1)
  putStrLn $ "Part 2: " ++ show (solution g2)
