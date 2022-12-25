{-# LANGUAGE OverloadedStrings #-}

module Day2 (problemA, problemB) where

import Aoc (Problem (..))

problemA :: Problem [Int] Int
problemA = Problem {parse = map calculatePoints . lines, solve = calories}
  where
    calories = sum
    calculatePoints "A X" = 1 + 3 -- Rock Rock
    calculatePoints "A Y" = 2 + 6 -- Rock Paper
    calculatePoints "A Z" = 3 + 0 -- Rock Scissor
    calculatePoints "B X" = 1 + 0 -- Paper Rock
    calculatePoints "B Y" = 2 + 3 -- Paper Paper
    calculatePoints "B Z" = 3 + 6 -- Paper Scissor
    calculatePoints "C X" = 1 + 6 -- Scissor Rock
    calculatePoints "C Y" = 2 + 0 -- Scissor Paper
    calculatePoints "C Z" = 3 + 3 -- Scissor Scissor
    calculatePoints _ = -10000

problemB :: Problem [Int] Int
problemB = Problem {parse = map calculatePointsB . lines, solve = calories}
  where
    calories = sum
    calculatePointsB "A X" = 3 + 0 -- Rock Lose
    calculatePointsB "A Y" = 1 + 3 -- Rock Draw
    calculatePointsB "A Z" = 2 + 6 -- Rock Win
    calculatePointsB "B X" = 1 + 0 -- Paper Lose
    calculatePointsB "B Y" = 2 + 3 -- Paper Draw
    calculatePointsB "B Z" = 3 + 6 -- Paper Win
    calculatePointsB "C X" = 2 + 0 -- Scissor Lose
    calculatePointsB "C Y" = 3 + 3 -- Scissor Draw
    calculatePointsB "C Z" = 1 + 6 -- Scissor Win
    calculatePointsB _ = -10000
