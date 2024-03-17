module Model where
import Graphics.Gloss

data NCTState = EmptyState
  | ChooseBase
  {
    imageScale  :: Float,
    screenSize  :: (Int, Int),
    pictureSize :: (Int, Int),
    neg         :: Picture,
    loc         :: Point
  }
  | Result
  {
    neg         :: Picture
  }