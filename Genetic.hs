module Genetic where

import Data.List (genericLength)
import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, getRandom, getRandomR)
import Nock

type MaxDepth = Integer
type Fitness = Double
type Problem = Noun -> Fitness


-- Some utilities for manipulating Nock trees

depth :: Noun -> Integer
depth n = case n of
            A x    -> 1
            x :> y -> 1 + max (depth x) (depth y)

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
replaceBranchAt noun ix newNoun = replace noun 1
  where replace tree i | i > ix    = tree
                       | i == ix   = newNoun
                       | otherwise = case tree of
                            A x    -> tree
                            x :> y -> replace x (2 * i) :> replace y (2 * i + 1)


-- Utilities for random generation of Nock programs

randomAtom :: MonadRandom m => m Noun
randomAtom = liftM (A . num) getRandom
  where num :: Double -> Integer
        num n | n < 0.35  = 0
              | n < 0.40  = 1
              | n < 0.45  = 2
              | n < 0.50  = 3
              | n < 0.55  = 4
              | n < 0.60  = 5
              | n < 0.65  = 6
              | n < 0.70  = 7
              | n < 0.75  = 8
              | n < 0.80  = 9
              | otherwise = 9 + floor (-log (1.0 - n) / (1.0/20) + 1.0)

randomCell :: MonadRandom m => MaxDepth -> m Noun
randomCell r | r <= 1    = randomAtom
             | otherwise = getRandom >>= makeCell
  where makeCell :: MonadRandom m => Double -> m Noun
        makeCell n | n < 0.40  = randomAtom
                   | otherwise = do
                        a <- randomCell (r - 1)
                        b <- randomCell (r - 1)
                        return $ a :> b

randomExpr :: MonadRandom m => MaxDepth -> m Noun
randomExpr r | r <= 1    = randomAtom
             | otherwise = getRandom >>= makeExpr
  where makeExpr :: MonadRandom m => Double -> m Noun
        makeExpr n | n < 0.05  = (A 0 :>) `liftM` randomExpr (r - 1)
                   | n < 0.10  = (A 1 :>) `liftM` randomExpr (r - 1)
                   | n < 0.15  = (A 4 :>) `liftM` randomExpr (r - 1)
                   | n < 0.20  = (A 5 :>) `liftM` randomExpr (r - 1)
                   | n < 0.25  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 2 :> b :> c)
                   | n < 0.30  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 3 :> b :> c)
                   | n < 0.35  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 7 :> b :> c)
                   | n < 0.40  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 8 :> b :> c)
                   | n < 0.45  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 9 :> b :> c)
                   | n < 0.50  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 2)
                        return $ (A 10 :> b :> c)
                   | n < 0.55  = do
                        b <- randomExpr (r - 2)
                        c <- randomExpr (r - 3)
                        d <- randomExpr (r - 3)
                        return $ (A 6 :> b :> c :> d)
                   | n < 0.75  = randomCell r
                   | otherwise = randomAtom


-- Basic genetic programming operations

pointMutate :: MonadRandom m => Noun -> m Noun
pointMutate n = do
    let ai = atomIndicies n
    r <- getRandomR (0, length ai - 1)
    newAtom <- randomAtom
    replaceBranchAt n (ai !! r) `liftM` randomAtom

crossover :: MonadRandom m => Noun -> Noun -> m Noun
crossover n1 n2 = do
    let bs = cells n2
    let ci = cellIndicies n1
    r <- getRandomR (0, length ci - 1)
    b <- (bs !!) `liftM` getRandomR (0, length bs - 1)
    return $ replaceBranchAt n1 (ci !! r) b

headlessCrossover :: MonadRandom m => Noun -> m Noun
headlessCrossover n = randomCell (depth n) >>= crossover n


-- Some programs to try and evolve

decrement :: [Integer] -> Problem
decrement inputs prog = (sum . map score) inputs / genericLength inputs
  where score n = case nock (A n :> prog) of
            Left x         -> 0.0
            Right (x :> y) -> 0.0
            Right (A x)    -> if x == (n - 1) then 1.0 else 0.0

cat :: [Noun] -> Problem
cat inputs prog = (sum . map score) inputs / genericLength inputs
  where score i = case nock (i :> prog) of
            Left x  -> 0.0
            Right x -> if x == i then 1.0 else 0.0
