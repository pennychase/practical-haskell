{-# LANGUAGE PatternSynonyms #-}

module Chapter3.Ranges 
    ( Range()
    , range
    , RangeObs(..)
    , r
    , pattern R) where


-- Range data type
-- Don't want to export constructor because we want to use range validate
data Range = Range Integer Integer 
        deriving Show

range a b = if a <= b
            then Range a b
            else error "a must be <= b"


-- Use View Patterns
-- Doesn't stop invalid input if construct RangeObs directly

data RangeObs = RO Integer Integer
        deriving Show

r :: Range -> RangeObs
r (Range a b) = RO a b

-- Use Pattern Synonyms
pattern R :: Integer -> Integer -> Range
pattern R a b <- Range a b
    where R a b = range a b