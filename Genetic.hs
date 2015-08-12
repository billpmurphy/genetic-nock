module Genetic where

import Data.List (genericLength)
import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, getRandom)
import Nock

type MaxDepth = Int
type Fitness = Double
type Problem = Noun -> Fitness


-- Random generation of Nock programs

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
randomCell 0 = randomAtom
randomCell r = getRandom >>= makeCell
  where makeCell :: MonadRandom m => Double -> m Noun
        makeCell n | n < 0.30  = randomAtom
                   | otherwise = do
                        a <- randomCell (r - 1)
                        b <- randomCell (r - 1)
                        return $ a :> b


-- Some programs to try and evolve

decrement :: [Integer] -> Problem
decrement inputs prog = (sum . map score) inputs / genericLength inputs
  where score n = case nock (A n :> prog) of
            Left x         -> 0.0
            Right (x :> y) -> 0.0
            Right (A x)    -> if x == (1 + n) then 1.0 else 0.0

cat :: [Noun] -> Problem
cat inputs prog = (sum . map score) inputs / genericLength inputs
  where score i = case nock (i :> prog) of
            Left x  -> 0.0
            Right x -> if x == i then 1.0 else 0.0
