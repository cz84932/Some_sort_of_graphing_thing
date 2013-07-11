import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.List
import Control.Applicative
import GHC.Float
import qualified KdTree2 as Kd
import WeightedPoint3_2

main = do
    dataFile <- readFile "alotofrandompoints.txt"
    let windowWidth = 800 :: Int
        windowHeight = 500 :: Int  
        edgeSpace = 50 :: Int
        initialScaleFactor = 5 :: ScaleFactor
        initialDataLength = 10 :: Int
--this criterion finds the closest point in the tree to some point and returns whether the two points are within 10 units of each other
        criterion :: (Eq p, Kd.Point p) => Kd.KdTree p -> p -> Maybe p
        criterion tree point = let Just nearestPoint = Kd.nearestNeighbor tree point in if Kd.dist2 point nearestPoint < 100 then Just nearestPoint else Nothing        
    let dataPoints = map read $ lines dataFile :: [(Double, Double, Double)]
        (dataList', toAdd') = splitAt initialDataLength dataPoints
        dataList = map (\(a,b,c) -> RatPoint01 $ RatPoint [a,b] 0 Nothing c) dataList'
        toAdd = map (\(a,b,c) -> RatPoint01 $ RatPoint [a,b] 0 Nothing c) toAdd'
    let convert :: ScaleFactor -> [Double] -> Picture
        convert scaleFactor [w, x, y] = Translate 
            (double2Float $ (fromIntegral $ windowWidth - edgeSpace) * (x - 0.5)) 
            (double2Float $ (fromIntegral $ windowHeight - edgeSpace) * (y - 0.5)) $ 
            circleSolid $ scaleFactor * (double2Float $ sqrt w)
    let normalization = getNormalizationList dataList
        initialWorld = World (pictures $ map ((convert initialScaleFactor) . normalize normalization) dataList) 
                             blank 
                             0
                             (normalization, initialScaleFactor, Kd.fromList dataList)
        toGraph x = pictures [background x, rectangleWire (fromIntegral $ windowWidth-edgeSpace) (fromIntegral $ windowHeight-edgeSpace), lastPoint x]                   
        handleEvent (EventKey (SpecialKey KeySpace) Down (Modifiers Up Up Up) _) (World a b c d) = if null (drop c toAdd) then World a blank c d else
            let newPoint = head (drop c toAdd)
                Normalization xm xM ym yM = updateNormalization newPoint (e1 d)
                relativeScale = double2Float $ minimum [((maxX $ e1 d) - (minX $ e1 d))/(xM - xm),
                                                        ((maxY $ e1 d) - (minY $ e1 d))/(yM - ym)]
                newScale = relativeScale * (e2 d)
                newNormalization = Normalization xm 
                                                 (xm + (1 / float2Double relativeScale) * ((maxX $ e1 d) - (minX $ e1 d)))
                                                 ym
                                                 (ym + (1 / float2Double relativeScale) * ((maxY $ e1 d) - (minY $ e1 d)))
                newPictureSingle = color red $ convert newScale $ normalize newNormalization newPoint
                addToPoint = criterion (e3 d) newPoint
                newPictureWeighted1 = case addToPoint of Nothing -> color blue $ convert newScale $ normalize newNormalization newPoint
                                                         Just point -> color green $ convert newScale $ combine1 (normalize newNormalization newPoint) (normalize newNormalization point)
                newPictureWeighted2 = color black $ case addToPoint of Nothing -> convert newScale $ normalize newNormalization newPoint
                                                                       Just point -> convert newScale $ combine1 (normalize newNormalization newPoint) (normalize newNormalization point)
                erase = if addToPoint == Nothing then blank else let Just point = addToPoint in color white $ convert (e2 d) $ normalize (e1 d) point
                newBackground = if newNormalization == e1 d then pictures [a, erase]
                                                            else translate (double2Float $ (minX (e1 d) + maxX (e1 d) - minX newNormalization - maxX newNormalization) / 2 * ((fromIntegral (windowWidth - edgeSpace)) / (maxX newNormalization - minX newNormalization))) 
                                                                           (double2Float $ (minY (e1 d) + maxY (e1 d) - minY newNormalization - maxY newNormalization) / 2 * ((fromIntegral (windowHeight - edgeSpace)) / (maxY newNormalization - minY newNormalization))) $ 
                                                                           scale relativeScale relativeScale $ pictures [a, erase]
                newTree = case addToPoint of Nothing -> Kd.insert (e3 d) newPoint
                                             Just x -> (if even c then lumpWith2 else lumpWith1) (e3 d) x newPoint
            in World (pictures [newBackground,color black newPictureWeighted2]) 
                     (pictures [newPictureSingle,newPictureWeighted1]) 
                     (c + 1)
                     (newNormalization, newScale, newTree)
        handleEvent _ world = world
        nextStep _ = id                
    play (InWindow "Window" (windowWidth,windowHeight) (10,10)) white 10 initialWorld toGraph handleEvent nextStep    

type ScaleFactor = Float
data World a = World {background :: Picture, lastPoint :: Picture, pointsAdded :: Int, internal :: (Normalization, ScaleFactor, Kd.KdTree a)}

data RatPoint = RatPoint {ratPoint :: [Double], ratLocation :: Double, ratChocolate :: Maybe Double, ratPointWeight :: Double} deriving (Show, Eq)
newtype RatPoint01 = RatPoint01 {getRatPoint01 :: RatPoint} deriving (Show, Eq)
instance Kd.Point RatPoint01 where
    dimension _ = 2
    coord 0 = (!! 0) . ratPoint . getRatPoint01
    coord 1 = (!! 1) . ratPoint . getRatPoint01
instance WeightedPoint RatPoint01 where
    weight = ratPointWeight . getRatPoint01
    combine (RatPoint01 (RatPoint a1 b1 c1 d1)) (RatPoint01 (RatPoint a2 b2 c2 d2)) = 
        let weightedSum = (\x y -> ((d1 * x) + (d2 * y)) / (d1 + d2)) in RatPoint01 $ RatPoint
        (zipWith weightedSum a1 a2)
        (weightedSum b1 b2)
        ((Just weightedSum) <*> c1 <*> c2)
        (d1 + d2)

e1 (a,b,c) = a
e2 (a,b,c) = b
e3 (a,b,c) = c 
    
data Normalization = Normalization {minX :: Double, maxX :: Double, minY :: Double, maxY :: Double} deriving (Show,Eq)

getNormalizationList :: Kd.Point p => [p] -> Normalization
getNormalizationList [] = error "cannot get normalization for empty list"
getNormalizationList (p:ps) = foldr updateNormalization (Normalization (Kd.coord 0 p) (Kd.coord 0 p) (Kd.coord 1 p) (Kd.coord 1 p)) ps

updateNormalization :: Kd.Point p => p -> Normalization -> Normalization
updateNormalization p (Normalization a b c d) =
            let a' = if (Kd.coord 0 p) < a then (Kd.coord 0 p) else a
                b' = if (Kd.coord 0 p) > b then (Kd.coord 0 p) else b
                c' = if (Kd.coord 1 p) < c then (Kd.coord 1 p) else c
                d' = if (Kd.coord 1 p) > d then (Kd.coord 1 p) else d
            in Normalization a' b' c' d'
          
normalize :: WeightedPoint p => Normalization -> p -> [Double]
normalize normalization point = let [w, x, y] = essential point in 
    [w, (x - minX normalization)/xrange, (y - minY normalization)/yrange]
    where xrange = maxX normalization - minX normalization
          yrange = maxY normalization - minY normalization            