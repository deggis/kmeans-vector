{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Math.KMeans
Copyright   :  (c) Alp Mestanogullari, 2011
License     :  BSD3
Maintainer  :  alpmestan@gmail.com
Stability   :  experimental

An implementation of the k-means clustering algorithm based on the efficient vector package.

-}

module Math.KMeans (kmeans) where

import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import Control.Arrow
import Data.Function (on)
import Debug.Trace
import Data.Ord (comparing)

--- * K-Means clustering algorithm

type Vec = V.Vector Double
data Cluster = Cluster {
  cid :: !Int,
  center :: !Vec
  }

type VecPair a = (a, Vec)

dropAssoc = snd
dropAssocs = map dropAssoc

distance :: Vec -> Vec -> Double
distance u v = V.sum $ V.zipWith (\a b -> (a - b)^2) u v

partitionPoints :: Int -> [VecPair a] -> [[VecPair a]]
partitionPoints k vs = go vs
  where go vs = case L.splitAt n vs of
          (vs', []) -> [vs']
          (vs', vss) -> vs' : go vss
        n = (length vs + k - 1) `div` k

computeClusters :: [[VecPair a]] -> [Cluster]
computeClusters = zipWith Cluster [0..] . map (f . dropAssocs)
  where f (x:xs) = let (n, v) = L.foldl' (\(k, s) v' -> (k+1, V.zipWith (+) s v')) (1, x) xs
                   in (V.map (\x -> x / fromIntegral n) v)

regroupPoints :: [Cluster] -> [VecPair a] -> [[VecPair a]]
regroupPoints clusters points = go points
  where go points = map (map snd) . L.groupBy ((==) `on` fst) . L.sortBy (comparing fst) $ map (\p -> (closest p, p)) points
        closest p = cid $ L.minimumBy (comparing (distance (dropAssoc p) . center)) clusters

kmeansStep :: [VecPair a] -> [[VecPair a]] -> [[VecPair a]]
kmeansStep points pgroups = regroupPoints (computeClusters pgroups) points

kmeansAux :: [VecPair a] -> [[VecPair a]] -> [[VecPair a]]
kmeansAux points pgroups =
  let pss = kmeansStep points pgroups
  in if map dropAssocs pss == map dropAssocs pgroups
    then pgroups
    else kmeansAux points pss

-- | Performs the k-means clustering algorithm
--   using trying to use 'k' clusters on the given list of points
kmeans :: Int -> [VecPair a] -> [[VecPair a]]
kmeans k points = kmeansAux points pgroups
  where pgroups = partitionPoints k points


