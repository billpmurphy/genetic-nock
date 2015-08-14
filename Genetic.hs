module Genetic where

import Data.List (genericLength)
import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, getRandom, getRandomR)
import Nock
import NockUtils

type MaxDepth = Integer
type Fitness = Double
type Problem = Noun -> Fitness


-- Random generation of Nock programs

randomAtom :: MonadRandom m => m Noun
randomAtom = liftM (A . num) getRandom
  where num :: Double -> Integer
        num n | n < 0.35  = 0
              | otherwise = floor (-log (1.0 - n) / (1.0/20) + 1.0)

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
        makeExpr n | n < 0.05  = (A 0 :>) `liftM` a
                   | n < 0.10  = (A 1 :>) `liftM` a
                   | n < 0.15  = (A 4 :>) `liftM` a
                   | n < 0.20  = (A 5 :>) `liftM` a
                   | n < 0.25  = (A 2 :>) `liftM` bc
                   | n < 0.30  = (A 3 :>) `liftM` bc
                   | n < 0.35  = (A 7 :>) `liftM` bc
                   | n < 0.40  = (A 8 :>) `liftM` bc
                   | n < 0.45  = (A 9 :>) `liftM` bc
                   | n < 0.50  = (A 6 :>) `liftM` bcd
                   | n < 0.55  = (A 10 :>) `liftM` bc
                   | n < 0.75  = randomCell r
                   | otherwise = randomAtom
        a = randomExpr (r - 1)
        bc = do
            b <- randomExpr (r - 2)
            c <- randomExpr (r - 2)
            return $ b :> c
        bcd = do
            b <- randomExpr (r - 2)
            c <- randomExpr (r - 3)
            d <- randomExpr (r - 3)
            return $ b :> c :> d


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
