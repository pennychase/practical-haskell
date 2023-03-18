{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6.KMeans where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform

-- Typeclasses

class  Ord v => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

instance Vector (Double, Double) where
    distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)

    centroid lst = let (u, v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst
                       n = fromIntegral $ length lst
                   in (u / n, v / n)

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id


-- kMeans 


{--- Initial Implementation
-- Note that in call to kMeans' threshold is an eta reduction
kMeans :: (Vector v, Vectorizable e v) 
        => (Int -> [e] -> [v])  -- initialization function
        -> Int                  -- number of centroids (i.e., k)
        -> [e]                  -- the information to be clustered
        -> Double               -- threshold
        -> (Int, [v])           -- final centroids and number of steps
kMeans i k points = kMeans' (0, (i k points)) points

kMeans' :: (Vector v, Vectorizable e v) => (Int, [v]) -> [e] -> Double -> (Int, [v])
kMeans' (steps, centroids) points threshold =
    let assignments = clusterAssignmentPhase centroids points
        oldNewCentroids = newCentroidPhase assignments
        newCentroids = map snd oldNewCentroids
    in  if shouldStop oldNewCentroids threshold
        then (steps, newCentroids)
        else kMeans' (steps+1, newCentroids) points threshold


clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                      in M.adjust (p:) chosenC m)
             initialMap
             points
    where
        compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)


newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v ,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold
-}

-- Lens implementation

data KMeansState e v = KMeansState  { _centroids :: [v]
                                    , _points :: [e]
                                    , _err :: Double
                                    , _threshold :: Double
                                    , _steps :: Int
                                    } deriving Show

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0         -- _err is initially Infinity (1.0/0.0)

kMeans :: (Vector v, Vectorizable e v) 
        => (Int -> [e] -> [v])  -- initialization function
        -> Int                  -- number of centroids (i.e., k)
        -> [e]                  -- the information to be clustered
        -> Double               -- threshold
        -> [v]                  -- final centroids
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
    let assignments = clusterAssignmentPhase state
        state1 = state & centroids.traversed
                       %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
        state2 = state1 & err .~ sum (zipWith distance (state^.centroids) (state1^.centroids))
        state3 = state2 & steps +~ 1
    in if state3^.err < state3^.threshold then state3 else kMeans' state3

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase state = 
        let centroids' = view centroids state
            points' = view points state
            initialMap = M.fromList $ zip centroids' (repeat [])
        in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids'
                          in M.adjust (p:) chosenC m)
                 initialMap
                 points'
        where
            compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)


-- simple function to initialize centroids
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- example
info = [(1,1),(1,2),(4,4),(4,5)] :: [(Double, Double)]

-- kMeans initializeSimple 2 info 0.001
-- => (2,[(1.0,1.5),(4.0,4.5)])

