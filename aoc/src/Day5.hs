{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Day5 (problemA) where

import Aoc (Problem (..))
import qualified Text.Parsec as Parsec
import Data.Either (rights)
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Data.List.Index (modifyAt, updateAt)
import Data.List(transpose)

data Move where
  Move :: { numberOf::Int, from::Int, to::Int}
          -> Move
  deriving Show

fullCrate::Parsec.Parsec String st Char
fullCrate = Parsec.oneOf "[" *> Parsec.anyChar <* Parsec.oneOf "]"

emptyCrate::Parsec.Parsec String st Char
emptyCrate = Parsec.oneOf " " *> Parsec.oneOf " " <* Parsec.oneOf " "

crate :: Parsec.Parsec String st Char
crate = fullCrate <|> emptyCrate

listOfCrates:: Parsec.Parsec String st [Char]
listOfCrates = Parsec.sepBy crate (Parsec.char ' ') <* Parsec.eof

positiveNumber::Parsec.Parsec String st Int
positiveNumber = read <$> Parsec.many1 Parsec.digit

move :: Parsec.Parsec String st Move
move = do
  _ <- Parsec.string "move"
  nof <- Parsec.oneOf " " *> positiveNumber
  _ <- Parsec.oneOf " " *> Parsec.string "from"
  f <- Parsec.oneOf " " *> positiveNumber
  _ <- Parsec.oneOf " " *> Parsec.string "to"
  t <- Parsec.oneOf " " *> positiveNumber
  _ <- Parsec.eof
  return $ Move nof f t

problemA :: Problem ([[Char]], [Move]) String
problemA = Problem {parsePuzzle = convertFile . lines, solvePuzzle = solve}
  where
    solve (l,m) = map head $ foldl moveCrate l m 
      where

        moveCrate::[[Char]]->Move->[[Char]]
        moveCrate l m = foldl doit l [0..(numberOf m-1)]
          where
            doit::[[Char]]->Int->[[Char]]
            doit l n = addCrate (removeCrate l (from m -1)) (head $ l!!(from m - 1)) (to m - 1)

        addCrate::[[Char]]->Char->Int->[[Char]]
        addCrate l c i = modifyAt i (c :)  l

        removeCrate::[[Char]]->Int->[[Char]]
        removeCrate l i = updateAt i (Just . tail) l

    convertFile::[String]->([[Char]], [Move])
    convertFile l = makeCrates $ splitOn [""] l

      where

        makeCrates::[[String]]->([[Char]], [Move])
        makeCrates (x:y:_) = (map removeSpaces $ transpose $ rights $ map (Parsec.parse listOfCrates "5.dat") x, rights $ map (Parsec.parse move "5.dat") y)
        makeCrates _ = undefined

        removeSpaces = filter (/= ' ')
