module InterpretGrammar where

import TransformGrammar
import qualified Drawing as D

import Data.Array

import System.Random hiding (split)
import qualified System.Random as R


-- Here is a reader monad for splittables
-- It is important for the monad to be lazy since we will use it lazily
-- But the reader monad is lazy by default, no?
newtype Reader r a = Reader { runReader :: r -> a }

class Splittable t where
  split :: t -> (t,t)

instance Splittable r => Monad (Reader r) where
  return a = Reader $ \_ -> a
  (Reader f) >>= g = Reader $ \r1 -> let (r2,r3) = split r1
			             in runReader (g (f r2)) r3

getEnv :: Reader r r
getEnv = Reader $ \r -> r

-- A drawing monad
type Draw a = Reader StdGen a

instance Splittable StdGen where
  split = R.split

getRandomIndex :: (Int,Int) -> Draw Int
getRandomIndex (low,high) 
    = Reader $ 
      \gen -> let (i,_) = next gen
	      in i `mod` (high - low) + low

-- Here comes the interpreter

createDrawing :: Gr -> Draw D.Drawing
createDrawing arr = do i <- getRandomIndex (bounds arr)
		       drawings <- mapM (drawFigure) (arr ! i)
		       return (D.Branch drawings)

drawFigure :: Figure -> Draw D.Drawing
drawFigure (Rule r trans) = do drawing <- createDrawing r
			       return (D.Transformation trans drawing)
drawFigure (Circle trans) = return (D.Transformation trans D.Circle)
drawFigure (Square trans) = return (D.Transformation trans D.Square)
