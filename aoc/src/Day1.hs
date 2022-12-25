{-# LANGUAGE OverloadedStrings #-}

module Day1 (problemA, problemB) where

import Aoc (Problem (..))
import Text.Read (readMaybe)
import Data.List (groupBy, sortBy)
import Data.Maybe (isJust, mapMaybe)

-- First part of day 1, using list comprehension
problemA :: Problem [Maybe Int] Int
problemA = Problem {parse = map readMaybe . lines, solve = calories}
  where
    calories l = maximum $ mapMaybe sumCalories (groupBy justs l)
    justs a b = isJust a && isJust b

    sumCalories :: [Maybe Int] -> Maybe Int
    sumCalories l = sum <$> sequence l

-- First part of day 1, using list comprehension
problemB :: Problem [Maybe Int] Int
problemB = Problem {parse = map readMaybe . lines, solve = calories}
  where
    calories l = sum $ take 3 $ sortBy (flip compare) $ mapMaybe sumCalories (groupBy justs l)
    justs a b = isJust a && isJust b

    sumCalories :: [Maybe Int] -> Maybe Int
    sumCalories l = sum <$> sequence l
