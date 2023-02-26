{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}

module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sort, group, groupBy)
import Data.Tree
import Data.Graph
import GHC.Real (infinity)
import Data.Monoid

-- Exercise 4.2 - For Map: write insert, delete, and adjust using alter

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k v m = M.alter (\_ -> Just v) k m

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (\_ -> Nothing) k m

adjust' :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' f k m = M.alter (\case  Nothing -> Nothing
                                Just v -> Just (f v))
                        k m

-- Client data types

data Client i = GovOrg  { clientId :: i
                        , clientName :: String
                        }
              | Company { clientId :: i
                        , clientName :: String
                        , person:: Person
                        , duty :: String
                        }
              | Individual { clientId :: i
                           , person :: Person
                           }
            deriving (Show, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     }
            deriving (Show, Ord)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
            deriving (Show, Eq, Ord, Enum)

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

clients :: [Client Integer]
clients = [ nasa, sarah, gil, apple, mercury, hhs, fourDegree, julie, chris, meta, fbi, gabby, allison ]

-- Exercise 4.3 - Classify clients

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients clients = foldr insertClient emptyClientMap clients
    where
        emptyClientMap :: M.Map ClientKind (S.Set (Client Integer))
        emptyClientMap = M.fromList [ (GovOrgKind, S.empty), (CompanyKind, S.empty), (IndividualKind, S.empty)]
        insertClient client clientMap = 
            case client of
                GovOrg {..} -> M.adjust (S.insert client) GovOrgKind clientMap
                Company {..} -> M.adjust (S.insert client) CompanyKind clientMap
                Individual {..} -> M.adjust (S.insert client) IndividualKind clientMap

classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' clients = M.fromList $ zip [GovOrgKind ..] (map S.fromList $ clientLists clients [] [] [])
    where
        clientLists [] gs cs is = [gs, cs, is]
        clientLists (client:rest) gs cs is =
            case client of
                GovOrg {..} -> clientLists rest (client:gs) cs is
                Company {..} -> clientLists rest gs (client:cs) is
                Individual {..} -> clientLists rest gs cs (client:is)

-- Trees

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
    let
        subtreesTraversed = concatMap (preOrder f) subtrees
    in f v : subtreesTraversed

postOrder :: (a -> b) -> Tree a -> [b]
postOrder f (Node v subtrees)  =
    let
        subtreesTraversed = concatMap (postOrder f) subtrees
    in subtreesTraversed <> [f v]

-- adapted from Data.Tree levels
breadthFirst :: (a -> b) -> Tree a -> [b]
breadthFirst f tree =
    concatMap (map (f . rootLabel)) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [tree]

t1 :: Tree Int
t1 = Node 1 [ Node 2 [ Node 3 [], Node 4 [], Node 5 [] ], Node 6 [ Node 7 [], Node 8 [], Node 9 [] ] ]

t2 :: Tree Int
t2 = Node 1 [ Node 2 [ Node 3 [], Node 4 [], Node 5 [Node 6 [], Node 7 [] ] ], Node 8 [ Node 9 [], Node 10 [], Node 11 [] ] ]

-- Graphs

-- Adjacency list (node, key, edge endpoints) for constructing graph showing precedence for constructing Time Machine
timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
    [ ("wood", "wood", ["walls"]), ("plastic", "plastic", ["walls", "wheels"])
    , ("aluminum", "aluminum", ["wheels", "door"]), ("walls", "walls", ["done"])
    , ("wheels", "wheels", ["done"]), ("door", "door", ["done"]), ("done", "done", [])]


-- Version of reachable that uses keys instead of Vertex indices for the vertex input and the list of reachable vertices
-- Call as: reachable' (graphFromEdges timeMachineGraph) "wood"
reachable' :: Ord k => (Graph, Vertex -> (a, k, [k]), k -> Maybe Vertex) -> k -> [k]
reachable' (g, nodeFromVertex, vertexFromKey) key = 
    let (Just v) = vertexFromKey key
    in map (\x -> let (_, k, _) = nodeFromVertex x in k) $ reachable g v

-- Edge list for time travel constraints
timeMachineTravel :: Graph
timeMachineTravel = buildG (103, 2013) [ (1302, 1614), (1614, 1302), (1302, 2013), (2013, 1302), (1614, 2013)
                                       , (2013, 1408), (1408, 1993), (1408, 917), (1993, 917), (917, 103), (103, 917)
                                       ]

-- Strongly connected components for TimeMachineTravel
-- map flatten  $ filter (\(Node { subForest = s }) -> s /= []) $ scc timeMachineTravel

-- Strongly connected components for TimeMachineGraph
-- map flattenSCC $ stronglyConnComp timeMachineGraph

-- Type Classes
-- Exercsie 4.5 - Eq instance for Client and Person

class Nameable a where
    name :: a -> String

instance Nameable (Client i) where
    name Individual { person = Person { firstName = fn, lastName = ln} } =
        fn <> " " <> ln
    name client = clientName client

-- For all client types, two clients are equal if the ids and names are equal. For Company, the person and duty must also be equal.
-- So first check id and name. Then, if the two are companies, check person and duty; if the two clients are GovOrg or Individual 
-- they've already been compared by clientId and name; any other combination is False.
instance Eq i => Eq (Client i) where
    client1 == client2 =   clientId client1 == clientId client2 
                        && name client1 == name client2 
                        && case (client1, client2) of
                                (Company _ _ person1 duty1, Company _ _ person2 duty2) -> person1 == person2 && duty1 == duty2
                                (GovOrg _ _, GovOrg _ _) -> True
                                (Individual _ _, Individual _ _) -> True
                                (_, _) -> False

instance Eq Person where
    p1 == p2 = firstName p1 == firstName p2 && lastName p1 == lastName p2


-- Binary Trees
-- Exercise 4.7

data BinaryTree a =     TNode a (BinaryTree a) (BinaryTree a)
                    |   Leaf
                    deriving Show

treeFind :: Ord a => a -> BinaryTree a -> Maybe a 
treeFind e (TNode v l r) =
    case compare e v of
        EQ -> Just v
        LT -> treeFind e l
        GT -> treeFind e r
treeFind _ Leaf = Nothing

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert e n@(TNode v l r) =
    case compare e v of
        EQ -> n
        LT -> TNode v (treeInsert e l) r
        GT -> TNode v l (treeInsert e r)
treeInsert e Leaf = TNode e Leaf Leaf

treeFromList :: Ord a => [a] -> BinaryTree a
treeFromList xs = foldl (flip treeInsert) Leaf xs 

treeFlatten :: BinaryTree a -> [a]
treeFlatten Leaf = []
treeFlatten (TNode v l r) = treeFlatten l <> [v] <> treeFlatten r

concatTree :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
concatTree t1 t2 = foldl (flip treeInsert) t2 (treeFlatten t1)

-- TRavel Guide Price

data TravelGuide =
    TravelGuide { title :: String
                , authors :: [String]
                , price :: Double
                }
                deriving (Show, Eq, Ord)

newtype TGByPrice = TGByPrice TravelGuide deriving (Show, Eq)

getPrice :: TGByPrice -> Double
getPrice (TGByPrice tg) = price tg 

instance Ord TGByPrice where
    (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
        p1 < p2 || (p1 == p2 && (t1 == t2 || (t1 == t2 && a1 <= a2)))

-- Binary Tree with Monoidal Cache

data BinaryTreeC v c =  NodeC v c (BinaryTreeC v c) (BinaryTreeC v c)
                      | LeafC
                      deriving (Show, Eq, Ord)

-- insert into tree and cache the smallest value
treeInsertC :: (Ord v, Monoid c) => v -> c -> BinaryTreeC v c -> BinaryTreeC v c
treeInsertC v c (NodeC v1 c1 l r) =
    case compare v v1 of
        EQ -> NodeC v1 c1 l r
        LT -> let newLeft = treeInsertC v c l
                  newCache = c1 <> cached newLeft <> cached r
              in NodeC v1 newCache newLeft r
        GT -> let newRight = treeInsertC v c r
                  newCache = c1 <> cached l <> cached newRight
              in NodeC v1 newCache l newRight
treeInsertC v c LeafC = NodeC v c LeafC LeafC

cached :: Monoid c => BinaryTreeC v c -> c
cached (NodeC _ c _ _) = c
cached LeafC = mempty

newtype Min = Min Double deriving Show

instance Semigroup Min where
    Min x <> Min y = Min $ min x y

instance Monoid Min where
    mempty = Min $ fromRational infinity


-- data
tg1 = TGByPrice (TravelGuide "Middle Ages" ["Connie Mack"] 10)
tg2 = TGByPrice (TravelGuide "The Middle Kingdom" ["Wallis Budge"] 25)
tg3 = TGByPrice (TravelGuide "The Old Kingdomn" ["George Reisner", "Peter der Manuelian"] 25)
tg4 = TGByPrice (TravelGuide "Late Antiquity" ["Peter Brown"] 15)
tg5 = TGByPrice (TravelGuide "The Great War" ["Joe Smith"] 20)

-- Functions to create trees from a list of the above data
-- Can apply cached to see the cached value

-- Cache the minimum value
mkMinTGTree :: [TGByPrice] -> BinaryTreeC TGByPrice Min
mkMinTGTree = foldr (\n t -> treeInsertC n (Min $ getPrice n) t) LeafC 

-- Cache the sum
mkSumTGTree :: [TGByPrice] -> BinaryTreeC TGByPrice (Sum Double)
mkSumTGTree = foldr (\n t -> treeInsertC n (Sum $ getPrice n) t) LeafC

-- Functors
-- Exercise 4.8

data MyMaybe a = MyNothing | MyJust a
        deriving Show

instance Functor MyMaybe  where
    fmap f MyNothing = MyNothing
    fmap f (MyJust v) = MyJust $ f v

instance Functor BinaryTree where
    fmap f Leaf = Leaf
    fmap f (TNode v l r) = TNode (f v) (fmap f l) (fmap f r)

binMap :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
binMap f tree = treeFromList $ treeFlatten $ fmap f tree

-- Foldable
-- Exercise 4.9

instance Foldable MyMaybe where
    foldMap :: Monoid m => (a -> m) -> MyMaybe a -> m
    foldMap _ MyNothing = mempty
    foldMap f (MyJust x) = f x

instance Foldable BinaryTree where
    foldMap :: Monoid m => (a -> m) -> BinaryTree a -> m
    foldMap _ Leaf = mempty
    foldMap f (TNode v l r) = f v <> foldMap f l <> foldMap f r

