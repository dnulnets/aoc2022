{-# LANGUAGE OverloadedStrings #-}

module Day3 (problemA, problemB) where

import Aoc (Problem (..))
import Data.Set (Set, fromList, intersection)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List.Split (chunksOf)

problemA :: Problem [([Int],[Int])] Int
problemA = Problem {parsePuzzle = map makeRucksack . lines, solvePuzzle = review}
  where
    review = sum . map (sum . toList . wrong)
      where
        wrong (ll,rr) = intersection (fromList ll) (fromList rr)

    makeRucksack::String->([Int], [Int])
    makeRucksack s = splitAt (div (length s) 2) (map (priority . ord) s)

      where

        priority::Int->Int
        priority i  | i>=97 && i<=122 = i - 96
                    | i>=65 && i<=90 = i - ( 65 - 27)
                    | otherwise = 0


problemB :: Problem [[Int]] Int
problemB = Problem {parsePuzzle = map makeRucksack . lines, solvePuzzle = review}
  where
    review l = sum $ map findBadge $ chunksOf 3 l
      where
        findBadge::[[Int]]->Int
        findBadge ll = head $ toList $ foldl intersectitems (fromList [1..52]) ll
          where
            intersectitems::Set Int->[Int]->Set Int
            intersectitems a b = intersection a (fromList b)

    makeRucksack::String->[Int]
    makeRucksack = map (priority . ord)

      where

        priority::Int->Int
        priority i  | i>=97 && i<=122 = i - 96
                    | i>=65 && i<=90 = i - ( 65 - 27)
                    | otherwise = 0
