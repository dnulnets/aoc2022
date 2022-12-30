{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Day6 (problemA, problemB) where

import Aoc (Problem (..))
import Data.List (nub)

data SOM where
  SOM :: {soh :: Bool, msg::[Char], ix::Int } -> SOM
  deriving Show

solve::Int->String->SOM
solve n = foldl doit (SOM False [] 0)
  where
    doit::SOM->Char->SOM
    doit som c = if soh som then som else update
      where
        update::SOM
        update = SOM found (if ix som > (n-1) then tail (msg som) ++ [c] else msg som ++ [c]) (ix som + 1)

        found::Bool
        found = (ix som > (n-2)) && length (nub (tail (msg som) ++ [c])) == n
        
problemA :: Problem String SOM
problemA = Problem {parsePuzzle = id, solvePuzzle = solve 4}

problemB :: Problem String SOM
problemB = Problem {parsePuzzle = id, solvePuzzle = solve 14}
