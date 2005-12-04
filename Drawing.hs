module Drawing where

{- The drawing tree is a data structure which says what should be drawn where.
It consists of circles and squares and transformations of drawing trees.
-}

data Drawing 
    = Circle
    | Square
    | Transformation Transform Drawing
    | Branch [Drawing]

data Transform 
    = Transform { dx      :: Double,
		  dy      :: Double,
		  dsize   :: Double,
		  dbright :: Double,
		  drotate :: Double }

identityTransform = Transform { dx      = 0,
				dy      = 0,
				dsize   = 1,
				dbright = 1,
				drotate = 0 }
