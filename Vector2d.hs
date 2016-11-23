-- Created By:  Benjamin Elo
-- Date:        23 November 2016
--
--
-- Basic 2d vector module for Haskell

module Vector2d (V2d (V2d), sumV, addV, subV, magV, scaleV, normV, distV)

where

data V2d = V2d Float Float deriving (Show, Eq) 

sumV :: [V2d] -> V2d
sumV = foldl addV (V2d 0 0)

addV :: V2d -> V2d -> V2d
addV (V2d x1 y1) (V2d x2 y2) = V2d (x1+x2) (y1+y2)

subV :: V2d -> V2d -> V2d
subV (V2d x1 y1) (V2d x2 y2) = V2d (x1-x2) (y1-y2)

magV :: V2d -> Float
magV (V2d x y) =  sqrt (x^2 + y^2)

scaleV :: Float -> V2d -> V2d
scaleV a (V2d x y) = V2d (a*x) (a*y)

normV :: V2d -> V2d
normV v = scaleV (1/(magV v)) v

distV :: V2d -> V2d -> Float
distV v1 v2 = magV (subV v1 v2)