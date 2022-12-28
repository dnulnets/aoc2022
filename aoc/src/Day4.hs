{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Day4 (problemA, problemB, ElfPair(..)) where

import Aoc (Problem (..))
import qualified Text.Parsec as Parsec
import Data.Either (rights)

data ElfPair where
  ElfPair :: {lstart :: Int, lend :: Int, rstart :: Int, rend :: Int}
             -> ElfPair
  deriving Show

numberParser::Parsec.Parsec String st Int
numberParser = read <$> Parsec.many1 Parsec.digit

lineParser:: Parsec.Parsec String st ElfPair
lineParser = do
    ls <- numberParser
    le <- Parsec.oneOf "-" *> numberParser
    rs <- read <$> (Parsec.oneOf "," *> Parsec.many1 Parsec.digit)
    re <- read <$> (Parsec.oneOf "-" *> Parsec.many1 Parsec.digit <* Parsec.eof)
    return $ ElfPair ls le rs re

makeElfPair::String->Either Parsec.ParseError ElfPair
makeElfPair = Parsec.parse lineParser "4.dat"

problemA :: Problem [ElfPair] Int
problemA = Problem {parsePuzzle = rights . map makeElfPair . lines, solvePuzzle = review}
  where
    review = length . filter id . map contained
      where
        contained::ElfPair->Bool
        contained ep = (lend ep <= rend ep && lstart ep >= rstart ep) ||
                          (rend ep <= lend ep && rstart ep >= lstart ep)

problemB :: Problem [ElfPair] Int
problemB = Problem {parsePuzzle = rights . map makeElfPair . lines, solvePuzzle = review}
  where
    review = length . filter id . map overlap
      where
        overlap::ElfPair->Bool
        overlap ep = not $ rstart ep > lend ep || lstart ep > rend ep || rend ep < lstart ep || lend ep < rstart ep

