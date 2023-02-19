{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter3.Lists where

import Prelude hiding (takeWhile, dropWhile, span, elem)
import Data.List (sortBy, groupBy, sort, group)
import Data.Function (on)
import GHC.Exts

import Chapter2.Records

-- Implement List functions from Prelude and Data.List

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = foldr (\x (trues, falses) -> if f x
                            then (x:trues, falses) 
                            else (trues, x:falses))
                        ([],[])
                        xs

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find pred (x:xs) =
    if pred x
        then Just x
        else find pred xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile pred (x:xs) =
    if pred x
        then x : takeWhile pred xs
        else []

dropWhile :: (a -> Bool) -> [a]  -> [a]
dropWhile _ [] = []
dropWhile pred (x:xs) =
    if pred x
        then dropWhile pred xs
        else xs

span :: (a -> Bool) -> [a] -> ([a],[a])
span pred lst = go lst []
    where 
        go [] taken = (taken, [])
        go (x:xs) taken =
            if pred x
                then go xs (x:taken)
                else (taken, x:xs)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy f (x:xs) = x : (nubBy f $ filter (not . f x) xs)

elem :: (Eq a) => a -> [a] -> Bool
elem x lst = 
    case find (x ==) lst of
        Nothing -> False
        _otherwise -> True


-- Working with Clients

-- GovOrgR and CompanyR precede IndividualR
-- Ties broken by comparing names (firstName for individuals, clientName for organizations)
compareClient :: ClientR -> ClientR -> Ordering
compareClient (IndividualR {person = p1}) (IndividualR {person=p2}) =
    compare (firstName p1) (firstName p2)
compareClient (IndividualR {}) _ = GT
compareClient _ (IndividualR {}) = LT
compareClient c1 c2 = compare (clientRName c1) (clientRName c2)

companyDutiesAnalytics :: [ClientR] -> [String]
companyDutiesAnalytics = map (duty . head) .
                            sortBy (flip (compare `on` length)) .
                            groupBy ((==) `on` duty) .
                            sortBy (compare `on` duty) . 
                            filter isCompany
                        where
                            isCompany (CompanyR {}) = True
                            isCompany _ = False

companyAnalytics :: [ClientR] -> [(String, [(PersonR, String)])]
companyAnalytics clients = [(the clientRName, zip person duty)
                            | client@(CompanyR {..}) <- clients
                            , then sortWith by duty
                            , then group by clientRName using groupWith
                            , then sortWith by length client
                            ]


-- More data
pear'' = CompanyR "Pear Inc" 342 (PersonR "Errol" "Flynn" Male) "Director"
pear''' = CompanyR "Pear Inc" 342 (PersonR "Jim" "Dine" Male) "Director"
tannenhaus'' = CompanyR "Tannenhaus Clockworks" 123 (PersonR "Charlotte" "Doppler" Female) "Scientist"
fourDegree'' = CompanyR { clientRName = "4Degree"
                      , companyID = 321
                      , person = PersonR { firstName = "Jim"
                                         , lastName = "Smith" 
                                         , gender = Male
                                         }
                      , duty = "Chief Scientist"
                      } 
wormhole'' = CompanyR { clientRName = "Wormhole, Inc"
                    , companyID = 42
                    , person = PersonR { firstName = "Mr."
                                       , lastName = "Spock"
                                       , gender = Male
                                       }
                    , duty = "Director"
                    }