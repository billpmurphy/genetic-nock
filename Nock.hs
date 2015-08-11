module Nock where

-- A noun is an atom or a cell.  An atom is any natural number.
-- A cell is an ordered pair of nouns.
data Noun = A !Integer | !Noun :> !Noun deriving (Eq)

infixr 1 :>

instance Show Noun where
  show (A a)    = show a
  show    a     = "[" ++ showCell a ++ "]"
    where showCell (a :> b) = show a ++ " " ++ showCell b
          showCell a        = show a

-- Type representing the computed result or the error'd code
type Nock = Either (Char, Noun) Noun

-- Reduction rules
nock, wut, lus, tis, fas, tar :: Noun -> Nock

nock = tar

wut (a :> b)                     = return $ A 0
wut a                            = return $ A 1

lus (a :> b)                     = Left ('+', a :> b)
lus (A a)                        = return $ A (1 + a)

tis (a :> b) | a == b            = return $ A 0
             | otherwise         = return $ A 1
tis a                            = Left ('=', a)

fas (A 1 :> a)                   = return a
fas (A 2 :> a :> b)              = return a
fas (A 3 :> a :> b)              = return b
fas (A a :> b) | a > 3           = do x <- fas $ A (a `div` 2) :> b
                                      fas $ A (2 + (a `mod` 2)) :> x
fas a                            = Left ('/', a)

tar (a :> (b :> c) :> d)         = do x <- tar (a :> b :> c)
                                      y <- tar (a :> d)
                                      return $ x :> y
tar (a :> A 0 :> b)              = fas (b :> a)
tar (a :> A 1 :> b)              = return b
tar (a :> A 2 :> b :> c)         = do a1 <- tar (a :> b)
                                      a2 <- tar (a :> c)
                                      tar (a1 :> a2)
tar (a :> A 3 :> b)              = tar (a :> b) >>= wut
tar (a :> A 4 :> b)              = tar (a :> b) >>= lus
tar (a :> A 5 :> b)              = tar (a :> b) >>= tis
tar (a :> A 6 :> b :> c :> d)    = tar (a :> A 2 :> (A 0 :> A 1) :>
                                        A 2 :> (A 1 :> c :> d) :>
                                        (A 1 :> A 0) :> A 2 :>
                                        (A 1 :> A 2 :> A 3) :>
                                        (A 1 :> A 0) :> A 4 :> A 4 :> b)
tar (a :> A 7 :> b :> c)         = tar (a :> A 2 :> b :> A 1 :> c)
tar (a :> A 8 :> b :> c)         = tar (a :> A 7 :> ((A 7 :> (A 0 :> A 1) :> b) :>
                                        A 0 :> A 1) :> c)
tar (a :> A 9 :> b :> c)         = tar (a :> A 7 :> c :> A 2 :>
                                        (A 0 :> A 1) :> A 0 :> b)
tar (a :> A 10 :> (b :> c) :> d) = tar (a :> A 8 :> c :> A 7 :>
                                        (A 0 :> A 3) :> d)
tar (a :> A 10 :> b :> c)        = tar (a :> c)
tar a                            = Left ('*', a)
