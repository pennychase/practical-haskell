{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Chapter3.Exercises where

import Chapter2.DataTypes
import Chapter3.Ranges

-- Exercise 3-2: Working with filters

filterOnes :: [Int] -> [Int]
filterOnes = filter (== 1)

filterANumber :: Int -> [Int] -> [Int]
filterANumber n = filter (== n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

filterGovOrgs1 :: [Client] -> [Client]
filterGovOrgs1 clients = filter isGovOrg clients
    where
        isGovOrg client =
            case client of
                GovOrg _ -> True
                _otherwise -> False

filterGovOrgs2 :: [Client] -> [Client]
filterGovOrgs2 clients = 
    filter (\case (GovOrg _) -> True
                  _otherwise -> False)
            clients

-- Range imports

-- Doesn't compile because Range constructor isn't exported
-- prettyRange :: Range -> String
-- prettyRange rng = 
--     case rng of
--         Range a b -> "[" <> show a <> ", " <> show b "]"


-- RangeObs and View Patterns
-- Call as: prettyRange (range 2 4)
prettyRange :: Range -> String
prettyRange rng = 
    case rng of
        (r -> RO a b) -> "[" <> show a <> ", " <> show b <> "]"

-- Pattern Synonyms
prettyRange' :: Range -> String
prettyRange' rng = 
    case rng of
        R a b -> "[" <> show a <> ", " <> show b <> "]"


-- Exercise 3-3: Folds

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (n:ns) = n * myProduct ns

myProduct' :: [Int] -> Int
myProduct' = foldr (*) 1

myAll :: [Bool] -> Bool
myAll [] = True
myAll (b:bs) = b && myAll bs

myAll' :: [Bool] -> Bool
myAll' = foldr (&&) True 

myMinimumBy :: (Int -> Int) -> [Int] -> Int
myMinimumBy f xs = foldr1 (\x y -> if f x < f y then x else y) xs

minimumClient :: [Client] -> Maybe Client
minimumClient []  = Nothing
minimumClient (c:cs) = go cs c
    where 
        go [] res = Just res
        go (c:cs) res =
            if length (clientName c) < length (clientName res)
                then go cs c
                else go cs res

minimumClient':: [Client] -> Client
minimumClient' clients = foldr1 (\x y -> if (length $ clientName x) < (length $ clientName y) then x else y) clients