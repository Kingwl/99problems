module Problem99Part2 where

import Problem99Part1
import Data.List
import Data.Foldable

data Encode a = Signal a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Encode a]
encodeModified xs = map (\(count, c) -> case count of
    1 -> Signal c
    _ -> Multiple count c 
  ) $ encode xs

decodeModified :: (Eq a) => [Encode a] -> [a]
decodeModified xs = foldl' (\o -> \x -> case x of
  (Signal x) -> o ++ [x]
  (Multiple c x) -> o ++ (replicate c x)) [] xs

dupli :: (Foldable t) => t a -> [a]
dupli xs = foldMap (\x -> [x, x]) xs 

repli :: (Foldable t) => t a -> Int -> [a]
repli xs c = foldMap (\x -> replicate c x) xs 

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n $ n - 1
dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n 0 = dropEvery' xs n $ n - 1
dropEvery' (x:xs) n c = (x:(dropEvery' xs n $ c - 1))

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs d t = (drop (d - 1) . take t) xs

rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = let (first, last) = splitAt n xs in last ++ first
  | otherwise = let (first, last) = splitAt (length xs + n) xs in last ++ first

removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "empty list"
removeAt 0 xs = (head xs, tail xs)
removeAt n xs = ((xs !! (n - 1)), take (n - 1) xs ++ drop n xs)
