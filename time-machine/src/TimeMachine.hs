module TimeMachine where

class Priceable a where
    price :: a -> Double

instance Priceable TimeMachine where
    price = tmPrice

instance Priceable Book where
    price = bookPrice

data TimeMachine =
    TimeMachine { tmManufacturer :: String
                , tmModel :: Int
                , tmName :: String
                , tmDirection :: Direction
                , tmPrice :: Double
                }
    deriving Show 

data Direction = Past | Future | Both
    deriving Show

data Book = 
    Book { bookAuthor :: String
         , bookTitle :: String
         , bookPrice :: Double
         }
        deriving Show

data Tool =
    Tool { toolType :: ToolType
         , toolPrice :: Double
         }
        deriving Show

data ToolType = ScrewDriver | Wrench | Drill
        deriving Show


discount :: [TimeMachine] -> Double -> [TimeMachine]
discount tms rate = map (discount' rate) tms
    where
        discount' rate (TimeMachine mfr model name dir amount) = TimeMachine mfr model name dir (amount - rate*amount)

totalPrice :: Priceable p => [p] -> Double
totalPrice = foldr ((+) . price) 0

-- Example Time Machines

tardis = TimeMachine "Time Lords Inc" 42 "The Tardis" Both 1000000
timeSnitch = TimeMachine "Eva Time Travel" 101 "Time Snitch" Both 500750
quantumLeap = TimeMachine "US Government" 1970 "QL Accelerator" Past 2000000000
theTM = TimeMachine "HG Wells" 1860 "TM" Future 650999

wellsTM = Book "H.G. Wells" "The Time Machine" 10.0
drWho = Book "The Doctor" "Bopping About in the Tardis" 25.0
finney = Book "Jack Finney" "Time and Again" 15.0
willis = Book "Connie Willia" "Oxford Time Travel" 50.0

