module DrawCairo where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo (renderWithDrawable)

import Drawing

drawFigure dr area = renderWithDrawable area (renderDrawing dr)

renderDrawing :: Drawing -> Render ()
renderDrawing Circle = do arc 0 0 0.5 0 (2*pi)
                          fill
renderDrawing Square = do rectangle (-0.5) (-0.5) 1 1
                          fill
renderDrawing (Transformation tr dr) =
  do save
     rotate (drotate tr)
     scale (dsize tr) (dsize tr)
     translate (dx tr) (dy tr)
     renderDrawing dr
     restore
renderDrawing (Branch drs) = mapM_ renderDrawing drs
