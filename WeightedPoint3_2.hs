module WeightedPoint3_2 where

import KdTree2
import qualified Data.List as L
import Data.Function (on)

class Point a => WeightedPoint a where
    weight :: a -> Double
    combine :: a -> a -> a

--lumpWith takes a tree, a point in the tree, and a new point
--and returns a new tree where the two points have been combined
lumpWith1 :: (Eq a, WeightedPoint a) => KdTree a -> a -> a -> KdTree a
lumpWith1 tree pTree pAdd = insert (remove1 tree pTree) $ combine pTree pAdd

lumpWith2 :: (Eq a, WeightedPoint a) => KdTree a -> a -> a -> KdTree a
lumpWith2 tree pTree pAdd = insert (remove2 tree pTree) $ combine pTree pAdd
          
findPoint :: WeightedPoint a => (a -> Bool) -> KdTree a -> Maybe a
findPoint f tree = foldr (\x y -> if f x then Just x else y) Nothing (toList tree)

findPointByScore :: WeightedPoint a => (a -> Double) -> KdTree a -> a
findPointByScore f tree = L.maximumBy (compare `on` f) (toList tree)

--associates a (weighted) point with a score for each element on the tree
--and adds that point to the element with the highest score
--comparison function should take two points, the first of which is the point in the tree
addPointByScore1 :: (Eq a, WeightedPoint a) => (a -> a -> Double) -> KdTree a -> a -> KdTree a
addPointByScore1 f tree p = lumpWith1 tree (findPointByScore (`f` p) tree) p

addPointByScore2 :: (Eq a, WeightedPoint a) => (a -> a -> Double) -> KdTree a -> a -> KdTree a
addPointByScore2 f tree p = lumpWith2 tree (findPointByScore (`f` p) tree) p

--same but associates True/False rather than a score and adds to the leftmost element returning true  
addPoint1 :: (Eq a, WeightedPoint a) => (a -> a -> Bool) -> KdTree a -> a -> KdTree a
addPoint1 f tree p = case findPoint (`f` p) tree of 
    Nothing -> insert tree p
    Just p' -> lumpWith1 tree p' p
    
addPoint2 :: (Eq a, WeightedPoint a) => (a -> a -> Bool) -> KdTree a -> a -> KdTree a
addPoint2 f tree p = case findPoint (`f` p) tree of 
    Nothing -> insert tree p
    Just p' -> lumpWith2 tree p' p    

essential :: WeightedPoint p => p -> [Double]
essential p = (weight p):[coord n p | n <- [0..dimension p - 1]]
    
combine1 :: [Double] -> [Double] -> [Double]
combine1 (x:xs) (y:ys) = (x + y) : (zipWith (\a b -> ((x * a) + (y * b)) / (x + y)) xs ys)