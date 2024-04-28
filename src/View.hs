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
    screenSize  = (sx, sy),
    pictureSize = ps,
    neg         = n,
    mouseLoc    = (mx, my),
    loc         = (x, y)
  })
  = pictures [positionPicture n choosebase, text (show x ++ ", " ++ show y)]

viewPure (Result converted is ss) = scale is is converted

positionPicture :: Picture -> NCTState -> Picture
positionPicture pic state@(ChooseBase {
  imageScale = is, 
  pictureSize = (px, py),
  screenSize = (sx, sy)
}) = translate tx ty (scale is is pic)
    where
      tx = (fromIntegral px/2 * is) - fromIntegral sx/2
      ty = (fromIntegral py/2 * is) - fromIntegral sy/2
