{-# LANGUAGE LambdaCase #-}

module Chapter6.Lenses where

import Lens.Micro.Platform

data Client i = GovOrg i String
              | Company i String Person String
              | Individual i Person
        deriving Show

data Person = Person String String
        deriving Show

-- Lenses by hand

-- Simple lenses dor Person
-- Define the setter and getter functions for each lens
-- Call getter: person^.firstname
-- Call setter: firstName .~ "NewName" $ person
firstName :: Lens' Person String
firstName = lens (\(Person f _) -> f)
                 (\(Person _ l) newF -> Person newF l)
lastName :: Lens' Person String
lastName = lens (\(Person _ l) -> l)
                 (\(Person f _) newL -> Person f newL)
fullName :: Lens' Person String
fullName = lens (\(Person f l) -> f <> " " <> l)
                (\_ newFullName -> case words newFullName of
                                        f:l:_ -> Person f l
                                        _ -> error "Incorrect name")

-- Full lens for Client identifier (can change the type when setting)
identifier :: Lens (Client i) (Client j) i j
identifier = lens (\case (GovOrg i _) -> i
                         (Company i _ _ _) -> i
                         (Individual i _) -> i)
                  (\client newId -> case client of
                         GovOrg _ n -> GovOrg newId n
                         Company _ n p r -> Company newId n p r
                         Individual _ p -> Individual newId p)


-- Some clients

nasa = GovOrg 1001 "NASA"
fbi = GovOrg 1002 "FBI"
hhs = GovOrg 1003 "HHS"
apple = Company 1 "Apple" (Person "Tim" "Cook") "CEO"
mercury = Company 2 "Mercury" (Person "Rebecca" "Skinner") "Scientist"
fourDegree = Company 3 "Four Degree" (Person "Alejandro" "Serrano") "Scientist"
meta = Company 4 "Meta" (Person "Don" "Stewart") "Director"
chris = Individual 101 (Person "Chris" "Martin")
gabby = Individual 102 (Person "Gabriella" "Gonzales")
allison = Individual 103 (Person "Allison" "Gill")
julie = Individual 104 (Person "Julie" "Moronuki")
gil = Individual 105 (Person "Gil" "Mizrahi")
sarah = Individual 106 (Person "Sarah" "Tabor")