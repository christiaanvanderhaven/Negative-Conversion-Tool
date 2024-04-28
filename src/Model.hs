module Model where
import Graphics.Gloss
import Codec.Picture
import Codec.Picture.Types

data NCTState = EmptyState
  | ChooseBase
  {
    imageScale  :: Float,
    screenSize  :: (Int, Int),
    pictureSize :: (Int, Int),
    neg         :: Picture,
    juicyNeg    :: Image PixelRGB16,
    mouseLoc    :: Point,
    loc         :: (Int, Int)
  }
  | Result
  {
    neg         :: Picture,
    imageScale  :: Float,
    screenSize  :: (Int, Int)
  }