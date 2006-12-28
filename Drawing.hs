module Drawing where

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

pruneAtDepth :: Int -> Drawing -> Drawing
pruneAtDepth 0 _ = Branch []
pruneAtDepth n (Transformation t d) = Transformation t (pruneAtDepth (n-1) d)
pruneAtDepth n (Branch ds) = Branch (map (pruneAtDepth (n-1)) ds)
pruneAtDepth n d = d

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
  = BBox ((cos dr * x + sin dr * y)*ds + dx) 
         ((sin dr * x + cos dr * y)*ds + dy) 
         ((cos dr * w + sin dr * h)*ds) 
         ((sin dr * w + cos dr * h)*ds)

unionBBox :: BBox -> BBox -> BBox
unionBBox (BBox x1 y1 h1 w1) (BBox x2 y2 h2 w2)
  = BBox x y w h
  where x = min x1 x2
        y = min y1 y2
        w = max (x1 + w1) (x2 + w2) - x
        h = max (y1 + h1) (y2 + h2) - y
