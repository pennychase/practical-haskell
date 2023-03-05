{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.KMeans where

import Data.List
import qualified Data.Map as M

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

-- Note that in call to kMeans' threshold is an eta reduction
kMeans :: (Vector v, Vectorizable e v) 
        => (Int -> [e] -> [v])  -- initialization function
        -> Int                  -- number of centroids (i.e., k)
        -> [e]                  -- the information to be clustered
        -> Double               -- threshold
        -> [v]                  -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
    let assignments = clusterAssignmentPhase centroids points
        oldNewCentroids = newCentroidPhase assignments
        newCentroids = map snd oldNewCentroids
    in  if shouldStop oldNewCentroids threshold
        then newCentroids
        else kMeans' newCentroids points threshold


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

-- simple function to initialize centroids
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- example
info = [(1,1),(1,2),(4,4),(4,5)] :: [(Double, Double)]

-- kMeans initializeSimple 2 info 0.001

