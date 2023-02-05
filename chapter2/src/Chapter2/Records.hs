{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.Records (ConnOptions(), connDefault) where

import Data.Char (toUpper)

-- Clients

data ClientR =
      GovOrgR { clientRName :: String }
    | CompanyR { clientRName :: String
                , companyID :: Integer
                , person :: PersonR
                , duty :: String
                }
    | IndividualR { person :: PersonR
                  , getAds :: Bool }
    deriving Show

data Gender = Male | Female | Unknown
    deriving Show

data PersonR = 
    PersonR { firstName :: String
            , lastName :: String
            , gender :: Gender
            } deriving Show


greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn }} = "Hi, " <> fn
greet CompanyR { clientRName = c } = "Hi, " <> c
greet GovOrgR { clientRName = c } = "Hi, " <> c

-- Using RecordPuns
greet' :: ClientR -> String
greet' IndividualR { person = PersonR { firstName}} = "Hi, " <> firstName
greet' CompanyR { clientRName } = "Hi, " <> clientRName
greet' GovOrgR { clientRName } = "Hi, " <> clientRName

-- Using RecordWildCards
greet'' :: ClientR -> String
greet'' IndividualR { person = PersonR { .. }} = "Hi, " <> firstName
greet'' CompanyR { .. } = "Hi, " <> clientRName
greet'' GovOrgR { .. } = "Hi, " <> clientRName

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest}) =
    let newName = (toUpper initial):rest
    in p { firstName = newName}
nameInCapitals p@PersonR { firstName = "" } = p

-- Count gender using records

getPersonR :: ClientR -> Maybe PersonR
getPersonR GovOrgR {} = Nothing
getPersonR CompanyR { person } = Just person
getPersonR IndividualR { person } = Just person

data CountsR = CountsR { male :: Int
                     , female :: Int
                     , unknown :: Int
                     }
                deriving Show

updateCounts :: CountsR -> PersonR -> CountsR
updateCounts cs@CountsR { .. } PersonR { .. } =
    case gender of
        Male -> cs { male = male + 1 }
        Female -> cs { female = female + 1 }
        Unknown -> cs { unknown = unknown + 1 }

genderCount :: [ClientR] -> CountsR
genderCount = foldr (\cl cnts -> countGender cl cnts) (CountsR 0 0 0)
    where
        countGender :: ClientR -> CountsR -> CountsR
        countGender client counts =
            case getPersonR client of
                Nothing -> counts
                Just person -> updateCounts counts person



-- Example Clients
nasa = GovOrgR "Nasa"

pear = CompanyR "Pear Inc" 342 (PersonR "Jack" "Sparrow" Male) "CEO"
tannenhaus = CompanyR "Tannenhaus Clockworks" 123 (PersonR "H.G." "Tannenhaus" Male) "Director"
fourDegree = CompanyR { clientRName = "4Degree"
                      , companyID = 321
                      , person = PersonR { firstName = "Mr."
                                         , lastName = "Alejandro" 
                                         , gender = Male
                                         }
                      , duty = "Chief Scientist"
                      } 

smith = IndividualR (PersonR "Jack" "Smith" Unknown) True
doe = IndividualR (PersonR "Jane" "Doe" Female) False
sparrow = IndividualR (PersonR "Jack" "Sparrow" Male) False
bear = IndividualR (PersonR "Nana" "Bear" Female) True
alejandro = IndividualR (PersonR "Mr." "Alejandro" Male) True

-- Default Values

data ConnType = TCP | UDP deriving Show
data UseProxy = NoProxy | Proxy String deriving Show
data TimeOut = NoTimeOut | TimeOut Integer

data ConnOptions = 
    ConnOptions { connType :: ConnType
                , connSpeed :: Integer
                , connProxy :: UseProxy
                , connCaching :: Bool
                , connKeepAlive :: Bool
                , connTimeOut :: TimeOut
                }

connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut



-- Exercise 2.7 - Rewrite Time Machines with Records

-- Time Machines

data TimeMachine = TimeMachine { tmManufacturer :: String
                               , tmModel :: Int
                               , tmName :: String
                               , tmdirection :: Direction
                               , tmPrice :: Float
                           } deriving Show

data Direction = Past | Future | Both
    deriving Show

discount :: [TimeMachine] -> Float -> [TimeMachine]
discount tms rate = map (discount' rate) tms
    where
        discount' rate tm@TimeMachine { tmPrice } = tm { tmPrice = tmPrice - rate*tmPrice }

-- Example Time Machines

tardis = TimeMachine "Time Lords Inc" 42 "The Tardis" Both 1000000
timeSnitch = TimeMachine "Eva Time Travel" 101 "Time Snitch" Both 500750
quantumLeap = TimeMachine "US Government" 1970 "QL Accelerator" Past 2000000000
theTM = TimeMachine "HG Wells" 1860 "TM" Future 650999
