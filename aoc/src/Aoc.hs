{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Aoc (Problem(..), notImplemented) where

-- The problem datatype that contains a parser for the file and a solver for the problem
data Problem a b where
  Problem :: {parsePuzzle :: String -> a, solvePuzzle :: a -> b} -> Problem a b

-- The NotImplemented problem, whenever I have no tsolved it yet
notImplemented::Problem String String
notImplemented = Problem {parsePuzzle = const "", solvePuzzle = const "Not implemented yet!"}
