-- Created By:  Benjamin Elo
-- Date:        23 November 2016
--
--
-- This program cretes a non-deterministic fractal tree using the "space colinization" algorithm
-- For more information visit "http://www.sea-of-memes.com/LetsCode26/LetsCode26.html"(Michael Goodfellow)


import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Vector2d

data Branch          = Branch Position Direction [ChildBranch] deriving (Show, Eq)
data Leaf            = Leaf V2d                                deriving (Show)

type ChildBranch     = Branch
type Trunk           = Branch
type Direction       = V2d
type Distance        = Float
type Position        = V2d

-- Parameters to genrate the tree
growDistance         = 10.0
maxDistance          = 50.0
minDistance          = 10.0
numberOfLeaves       = 200

-- Initial Tree Value

main = do
    seed1 <- newStdGen
    seed2 <- newStdGen
    let trunk = (Branch (V2d 0 (-300)) (V2d 0 300) [])
    let leaves = removeReachedLeaves (generateLeaves numberOfLeaves seed1 seed2) trunk
    let model = (trunk, leaves)
    simulate (InWindow "Tree" (600, 800) (20,  20)) (makeColor 0.1 0.1 0.1 1) 5 model (\(branches, leaves) -> Pictures [drawLeaves green leaves , drawBranches white branches]) (growTrunk)

generateLeaves :: Int -> StdGen -> StdGen -> [Leaf]
generateLeaves n seed1 seed2 = [Leaf (V2d x y) | (x,y) <- zip rands1 rands2]
    where 
        rands1 = take n (randomRs (-150, 150) seed1)
        rands2 = take n (randomRs (0, 300) seed2)

growTrunk :: ViewPort -> Float -> (Trunk, [Leaf]) -> (Trunk, [Leaf])
growTrunk _ _ (trunk, leaves) = ((grow trunk updatedLeaves),updatedLeaves)
    where updatedLeaves = removeReachedLeaves leaves trunk

iterateGrow :: Int -> Trunk -> [Leaf] -> Trunk
iterateGrow 0 trunk _ = trunk
iterateGrow a trunk leaves = iterateGrow (a-1) (grow trunk leaves) (removeReachedLeaves leaves trunk)

grow :: Trunk -> [Leaf] -> Trunk
grow trunk leaves = grow' 1 trunk (growDirection trunk leaves)

grow' :: Int -> Trunk -> [Direction] -> Trunk
grow' a trunk dirs = head (insertBranches 0 [trunk] (map (scaleV growDistance) dirs))

growDirection :: Trunk -> [Leaf] -> [Direction]
growDirection trunk leaves = growDirection' branches leaves (zip [1..] branches)
    where branches = treeToList [trunk]

growDirection' :: [Branch] -> [Leaf] -> [(Int, Branch)] -> [Direction]
growDirection' _ _ [] = []
growDirection' trunk leaves ((i,b):bs) = (normGrowDir [if (dis < maxDistance) && (closestBranch l trunk == i) then (dir, dis) else ((V2d 0 0),0) | l <- leaves, let dis = distanceToBranch l b, let dir = directionToBranch b l]):(growDirection' trunk leaves bs)

getDir :: [(Direction, Distance)] -> [Direction]
getDir [] = []
getDir ((dir,_):xs) = dir:(getDir xs)

getDis :: [(Direction, Distance)] -> [Float]
getDis [] = []
getDis ((_,dis):xs) = dis:(getDis xs)

normGrowDir :: [(Direction, Distance)] -> Direction
normGrowDir xs 
        |  dis == 0 = V2d 0 0
        |  otherwise = normV (scaleV (1/dis) dir)
        where 
            dis = sum (getDis xs)
            dir = sumV (getDir xs)

insertBranches :: Int -> [Branch] -> [Direction] -> [Branch]
insertBranches _ [] _ = []
insertBranches a ((Branch p d cbs):bs) dirs 
        | dir /= V2d 0 0 = (Branch p d ((Branch (addV p d) dir []):(insertBranches (1+a) cbs dirs)):(insertBranches (1+a+(numberOfNodes' cbs)) bs dirs))
        | otherwise = (Branch p d (insertBranches (1+a) cbs dirs)):(insertBranches (1+a+(numberOfNodes' cbs)) bs dirs)
	where dir = (dirs !! a)

closestBranch :: Leaf -> [Branch] -> Int
closestBranch leaf branches = snd (minimum [((distanceToBranch leaf branch), i) | (branch, i) <- zip branches [1..]])

numberOfNodes :: Trunk -> Int
numberOfNodes trunk = numberOfNodes' [trunk]

numberOfNodes' :: [Branch] -> Int
numberOfNodes' []                       = 0
numberOfNodes' ((Branch _ _ []):bs)     = 1 + numberOfNodes' bs
numberOfNodes' ((Branch _ _ cbs):bs)    = 1 + (numberOfNodes' cbs) + (numberOfNodes' bs)

treeToList :: [Trunk] -> [Branch]
treeToList [] = []
treeToList ((b@(Branch _ _ cbs)):bs) = b:((treeToList cbs) ++ (treeToList bs))

directionToBranch :: Branch -> Leaf -> V2d
directionToBranch (Branch pb db _) (Leaf lp) = subV lp (addV pb db)

distanceToBranch :: Leaf -> Branch -> Float
distanceToBranch (Leaf lp) (Branch pb db _) = distV lp (addV pb db)

removeReachedLeaves :: [Leaf] -> Trunk -> [Leaf]
removeReachedLeaves leaves trunk = [leaf | leaf <- leaves, (minimum (map (distanceToBranch leaf) branches)) > minDistance]
    where branches = treeToList [trunk]

branchesToPictures :: Branch -> [Picture]
branchesToPictures (Branch (V2d x y) (V2d d1 d2) []) = [(Line [(x,y),(x+d1,y+d2)])]
branchesToPictures (Branch (V2d x y) (V2d d1 d2) branches) = (Line [(x,y),(x+d1,y+d2)]):(concat (map branchesToPictures branches))

drawBranches :: Color -> Branch -> Picture
drawBranches c branch = Color c (Pictures (branchesToPictures branch))

drawLeaves :: Color -> [Leaf] -> Picture
drawLeaves c leaves = Pictures [Color c ((Translate x y) (Circle 4.0))  | Leaf (V2d x y) <- leaves]

