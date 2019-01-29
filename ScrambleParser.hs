module ScrambleParser where

import Text.Parsec
import Rubik

direction :: Parsec String st Direction
direction = readDir <$> oneOf (concatMap show directions)

-- TODO: add number of rotations to the tuple
turn :: Parsec String st (Permutation, Direction, Rotation)
turn = do
  dir <- direction
  rotation <- option Clockwise (char '\'' >> return CounterClockwise)
  n <- option 1 (char '2' >> return 2)
  return (mconcat $ replicate n $ Permutation $ turnCube dir rotation, dir, rotation)

-- TODO: add number of rotations to the tuple
permutationList :: Parsec String st [(Permutation, Direction, Rotation)]
permutationList = turn `sepBy` space 

parseTurn :: String -> (Permutation, Direction, Rotation)
parseTurn s = case parse turn "" s of
                   Right t -> t
                   Left _ -> error "could not parse turn"

parsePermutations :: String -> Permutation
parsePermutations s = case parse permutationList "" s of
                           Right ps -> case unzip3 ps of
                                            (ps, _, _) -> mconcat ps
                           Left _ -> error "could not parse permutations"
