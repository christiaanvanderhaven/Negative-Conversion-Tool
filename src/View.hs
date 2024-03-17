module View where
import Model
import Graphics.Gloss

view :: NCTState -> IO Picture
view state = return (viewPure state)

viewPure :: NCTState -> Picture
viewPure EmptyState = Blank

viewPure choosebase@(ChooseBase 
  {
    imageScale  = is,
    screenSize  = ss,
    pictureSize = ps,
    neg         = n,
    loc         = (x, y)
  })
  = scale is is n