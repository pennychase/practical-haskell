{-# LANGUAGE ViewPatterns #-}

module Chapter2.DataTypes where

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
    deriving Show

data Gender = Male | Female | Unknown
    deriving Show

clientName :: Client -> String
clientName client =
    case client of
        GovOrg name -> name
        Company name _ _ _ -> name
        Individual (Person fname lname _) _ -> fname <> " " <> lname

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

getPerson :: Client -> Maybe Person
getPerson (GovOrg _) = Nothing
getPerson (Company _ _ person _) = Just person
getPerson (Individual person _) = Just person

-- Exercise 2.5 - Count genders of clients

data Counts = Counts Int Int Int
    deriving Show

updateCounts :: Counts -> Person -> Counts
updateCounts (Counts m f u) (Person _ _ Male) = Counts (m+1) f u
updateCounts (Counts m f u) (Person _ _ Female) = Counts m (f+1) u
updateCounts (Counts m f u) (Person _ _ Unknown) = Counts m f (u+1)

genderCount :: [Client] -> Counts
genderCount = foldr (\cl cnts -> countGender cl cnts) (Counts 0 0 0)
    where
        countGender :: Client -> Counts -> Counts
        countGender client counts =
            case getPerson client of
                Nothing -> counts
                Just person -> updateCounts counts person

-- View Patterns
specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

-- Example Clients
nasa = GovOrg "Nasa"

pear = Company "Pear Inc" 342 (Person "Jack" "Sparrow" Male) "CEO"
tannenhaus = Company "Tannenhaus Clockworks" 123 (Person "H.G." "Tannenhaus" Male) "Director"
fourDegree = Company "4Degree" 321 (Person "Mr." "Alejandro" Male) "Chief Scientist"

smith = Individual (Person "Jack" "Smith" Unknown) True
doe = Individual (Person "Jane" "Doe" Female) False
sparrow = Individual (Person "Jack" "Sparrow" Male) False
bear = Individual (Person "Nana" "Bear" Female) True
alejandro = Individual (Person "Mr." "Alejandro" Male) True

-- Time Machines

data TimeMachine = TimeMachine String Int String Direction Float
    deriving Show

data Direction = Past | Future | Both
    deriving Show

-- Exercise 5 - Discount list of time machines

discount :: [TimeMachine] -> Float -> [TimeMachine]
discount tms rate = map (discount' rate) tms
    where
        discount' rate (TimeMachine mfr model name dir amount) = TimeMachine mfr model name dir (amount - rate*amount)

-- Example Time Machines

tardis = TimeMachine "Time Lords Inc" 42 "The Tardis" Both 1000000
timeSnitch = TimeMachine "Eva Time Travel" 101 "Time Snitch" Both 500750
quantumLeap = TimeMachine "US Government" 1970 "QL Accelerator" Past 2000000000
theTM = TimeMachine "HG Wells" 1860 "TM" Future 650999

-- Default Values

data ConnType = TCP | UDP deriving Show
data UseProxy = NoProxy | Proxy String deriving Show
data TimeOut = NoTimeOut | TimeOut Integer deriving Show

data ConnOptions = 
    ConnOptions { connType :: ConnType
                , connSpeed :: Integer
                , connProxy :: UseProxy
                , connCaching :: Bool
                , connKeepAlive :: Bool
                , connTimeOut :: TimeOut
                } deriving Show

connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut


