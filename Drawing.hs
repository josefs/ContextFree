module Drawing where

import Control.Monad.Writer
import Data.List

{- The drawing tree is a data structure which says what should be drawn where.
It consists of circles and squares and transformations of drawing trees.
-}

data Drawing 
    = Circle
    | Square
    | Transformation Transform Drawing
    | Branch [Drawing]
      deriving Show

data Transform 
    = Transform { dx      :: Double,
		  dy      :: Double,
		  dsize   :: Double,
		  dbright :: Double,
		  drotate :: Double }
       deriving Show

identityTransform = Transform { dx      = 0,
				dy      = 0,
				dsize   = 1,
				dbright = 1,
				drotate = 0 }

-- Pruning.
-- Turns out to be trickier than I thought to get it elegant

pruneAtDepth :: Int -> Drawing -> Drawing
pruneAtDepth 0 _ = Branch []
pruneAtDepth n (Transformation t d) = Transformation t (pruneAtDepth (n-1) d)
pruneAtDepth n (Branch ds) = Branch (map (pruneAtDepth (n-1)) ds)
pruneAtDepth n d = d

data PruneInfo = Pruned | NotPruned deriving (Show, Eq)

instance Monoid PruneInfo where
  mempty = NotPruned
  mappend Pruned _ = Pruned
  mappend _ Pruned = Pruned
  mappend _ _      = NotPruned

pruneAtDepthM :: Int -> Drawing -> Writer PruneInfo Drawing
pruneAtDepthM 0 _ = do tell Pruned
                       return (Branch [])
pruneAtDepthM n (Transformation t d) = do dp <- pruneAtDepthM (n-1) d
                                          return (Transformation t dp)
pruneAtDepthM n (Branch ds) = fmap Branch $ mapM (pruneAtDepthM (n-1)) ds 
pruneAtDepthM n d = return d

pruneAtDepthB :: Int -> Drawing -> (Drawing,PruneInfo)
pruneAtDepthB i d = runWriter (pruneAtDepthM i d)

iteratePrune :: Int -> Drawing -> [Drawing]
iteratePrune step dr =
  map fst $
  takeWhile1 ((== Pruned) . snd) $
  map (\i -> pruneAtDepthB i dr) [step,2*step..]

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = [x]

-- Here's a bit nicer version. 
-- But it still is not as simple as I wish for

iteratePruneAux :: Int -> Int -> Drawing -> [Drawing]
iteratePruneAux m 0 d = Branch [] : iteratePruneAux m m d
iteratePruneAux m n (Transformation tr dr)
  = map (Transformation tr) $ iteratePruneAux m (n-1) dr
iteratePruneAux m n (Branch ds)
  = map Branch $ mytranspose $ map (iteratePruneAux m (n-1)) ds
iteratePruneAux m n d = [d]

ensureLength n [x] = replicate n x
ensureLength n (x:xs) = x : ensureLength (n-1) xs

mytranspose xs = transpose $
  map (ensureLength (maximum (map length xs))) xs

iteratePruneNew :: Int -> Drawing -> [Drawing]
iteratePruneNew n = iteratePruneAux n n

-- Calculating the bounding box around a drawing

data BBox = BBox { x :: !Double
                 , y :: !Double
                 , w :: !Double
                 , h :: !Double }
             deriving Show

boundingBox :: Drawing -> BBox
boundingBox (Transformation tr dr) = transformBBox tr (boundingBox dr)
boundingBox (Branch ds) = foldr1 unionBBox (map boundingBox ds)
boundingBox _ = BBox (-0.5) (-0.5) 1 1

transformBBox :: Transform -> BBox -> BBox
transformBBox (Transform dx dy ds _ dr) (BBox x y w h) 
  = BBox ((cos r * x + sin r * y)*ds + dx) 
         ((sin r * x + cos r * y)*ds + dy) 
         ((cos r * w + sin r * h)*ds) 
         ((sin r * w + cos r * h)*ds)
  where r = dr * pi / 180

unionBBox :: BBox -> BBox -> BBox
unionBBox (BBox x1 y1 h1 w1) (BBox x2 y2 h2 w2)
  = BBox x y w h
  where x = min x1 x2
        y = min y1 y2
        w = max (x1 + w1) (x2 + w2) - x
        h = max (y1 + h1) (y2 + h2) - y
