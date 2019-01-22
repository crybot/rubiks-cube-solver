module ScrambleParser where

import Text.Parsec
import Rubik

direction :: Parsec String st Direction
direction = readDir <$> oneOf (concatMap show directions)

turn :: Parsec String st Permutation
turn = do
  dir <- direction
  rotation <- option Clockwise (char '\'' >> return CounterClockwise)
  n <- option 1 (char '2' >> return 2)
  return $ mconcat $ replicate n $ Permutation $ turnCube dir rotation

permutationList :: Parsec String st [Permutation]
permutationList = turn `sepBy` space 

parsePermutations :: String -> Permutation
parsePermutations s = case parse permutationList "" s of
                           Right ps -> mconcat ps
                           Left _ -> error "could not parse permutations"
