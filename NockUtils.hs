module NockUtils where

import Nock
--
-- Some utilities for manipulating Nock trees

depth :: Noun -> Integer
depth n = case n of
            A x    -> 1
            x :> y -> 1 + max (depth x) (depth y)

branches :: Noun -> [Noun]
branches n = case n of
            A x    -> [n]
            x :> y -> [n] ++ atoms x ++ atoms y

atoms :: Noun -> [Noun]
atoms n = case n of
            A x    -> [n]
            x :> y -> atoms x ++ atoms y

cells :: Noun -> [Noun]
cells n = case n of
            A x    -> []
            x :> y -> [n] ++ cells x ++ cells y

atomIndicies :: Noun -> [Integer]
atomIndicies = findAIx 1
  where findAIx i n = case n of
            A x    -> [i]
            x :> y -> findAIx (2 * i) x ++ findAIx (2 * i + 1) y

cellIndicies :: Noun -> [Integer]
cellIndicies = findCIx 1
  where findCIx i n = case n of
            A x    -> []
            x :> y -> [i] ++ findCIx (2 * i) x ++ findCIx (2 * i + 1) y

replaceBranchAt :: Noun -> Integer -> Noun -> Noun
replaceBranchAt noun ix newNoun = rep noun 1
  where rep tree i | i > ix    = tree
                   | i == ix   = newNoun
                   | otherwise = case tree of
                        A x    -> tree
                        x :> y -> rep x (2 * i) :> rep y (2 * i + 1)
