{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Lenses where

import Lens.Micro.Platform

-- Clients

data Client i = GovOrg  { _identifier :: i
                        , _name :: String
                        }
              | Company { _identifier :: i
                        , _name :: String
                        , _person:: Person
                        , _duty :: String
                        }
              | Individual { _identifier :: i
                           , _person :: Person
                           }
            deriving Show

data Person = Person { _firstName :: String
                     , _lastName :: String
                     }
            deriving Show

-- Make lenses
makeLenses ''Client
makeLenses ''Person

fullName :: Lens' Person String
fullName = lens (\(Person f l) -> f <> " " <> l)
                ( \_ newFullName -> case words newFullName of
                                        f:l:_ -> Person f l
                                        _ -> error "Incorrect name")

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

-- Time Machines

data Direction = Past | Future | Both
    deriving Show

data TimeMachine =
    TimeMachine { _tmManufacturer :: String
                , _tmModel :: Int
                , _tmName :: String
                , _tmDirection :: Direction
                , _tmPrice :: Double
                }
    deriving Show 

-- Make Lenses
makeLenses ''TimeMachine

-- Change Price
changePrice :: Double -> TimeMachine -> TimeMachine
changePrice rate tm = tm & tmPrice %~ (\price -> price + price * rate)

changePrices :: Double -> [TimeMachine] -> [TimeMachine]
changePrices rate tms = tms & traversed.tmPrice %~ (\price -> price + price * rate)



-- Some time machines

tardis = TimeMachine "Time Lords Inc" 42 "The Tardis" Both 1000000
timeSnitch = TimeMachine "Eva Time Travel" 101 "Time Snitch" Both 550000
quantumLeap = TimeMachine "US Government" 1970 "QL Accelerator" Past 20000000
theTM = TimeMachine "HG Wells" 1860 "TM" Future 650000

