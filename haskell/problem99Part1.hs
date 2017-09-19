module Problem99Part1 where

import Data.List

lastElement :: [a] -> a
lastElement [] = error "empty list"
lastElement (x:[]) = x
lastElement (x:xs) = lastElement xs

butLastElement :: [a] -> a
butLastElement [] = error "empty list"
butLastElement (x:[]) = error "only one element"
butLastElement (x:y:[]) = x
butLastElement (x:y:xs) = butLastElement (y:xs)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt _ 0 = error "invalid index"
elementAt xs 1 = head xs
elementAt (x:xs) n = elementAt xs $ n - 1

lengthOfList :: [a] -> Int
lengthOfList [] = 0
lengthOfList (x:xs) = 1 + lengthOfList xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverseList xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (\o -> \n -> o ++ n) [] $ map (\x -> flatten x) xs

compress :: (Eq a) => [a] -> [a]
compress xs = compress' xs []
compress':: (Eq a) => [a] -> [a] -> [a]
compress' [] set = set
compress' (x:xs) set
  | x `elem` set = compress' xs set
  | otherwise = compress' xs (set ++ [x])

pack :: (Eq a) => [a] -> [[a]]
pack xs = pack'' (pack' xs [])

pack' :: (Eq a) => [a] -> [(a, Int)] -> [(a, Int)]
pack' [] set = set
pack' (x:xs) set = let idx = findIndex ((==x) . fst) set in case idx of
  Nothing -> pack' xs $ set ++ [(x, 1)]
  (Just i) -> pack' xs $ let (k, v) = set !! i in
    (take i set) ++ ([(k, v + 1)]) ++ (drop (i + 1) set)

pack'' :: (Eq a) => [(a, Int)] -> [[a]]
pack'' [] = []
pack'' ((k, v):xs) = [(replicate v k)] ++ (pack'' xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = reverseList $ encode' xs []

encode' :: (Eq a) => [a] -> [(Int, a)] -> [(Int, a)]
encode' [] set = set
encode' (x:xs) [] = encode' xs [(1, x)]
encode' (x:xs) set
    | x == last = encode' xs $ ((count + 1, last):(tail set))
    | otherwise = encode' xs $ ((1, x):set)
      where (count, last) = head set