module Controller where
import Model
import Conversions
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Juicy (fromImageRGB8)

step :: Float -> NCTState -> IO NCTState
step _ EmptyState = return EmptyState
step _ state = return state


input :: Event -> NCTState -> IO NCTState
input _ EmptyState = return EmptyState

input e@(EventKey _ _ _ _) state = inputKey e state
input e@(EventMotion _) state = inputMotion e state
input e@(EventResize (x, y)) state = return state { screenSize = (x, y)}


inputKey :: Event -> NCTState -> IO NCTState

-- Scaling
inputKey (EventKey (Char '=') Down _ _) state@(ChooseBase { imageScale = s }) = return state {imageScale = s * 2}
inputKey (EventKey (Char '-') Down _ _) state@(ChooseBase { imageScale = s }) = return state {imageScale = s / 2}
inputKey (EventKey (Char '=') Down _ _) state@(Result { imageScale = s }) = return state {imageScale = s * 2}
inputKey (EventKey (Char '-') Down _ _) state@(Result { imageScale = s }) = return state {imageScale = s / 2}

inputKey (EventKey (SpecialKey KeyEnter) Down _ _) state@(ChooseBase { loc = l, juicyNeg = jn, screenSize = ss }) = return (Result (fromImageRGB8 (imageConvertLoc jn l)) 0.25 ss)

inputKey (EventKey (MouseButton LeftButton) Down _ _) state@(ChooseBase { mouseLoc = m }) = 
  return state {loc = screenToPixel m state}

inputKey _ state = return state


inputMotion :: Event -> NCTState -> IO NCTState
inputMotion (EventMotion (x', y')) state@(ChooseBase { mouseLoc = (x, y)}) = do
  return state { mouseLoc = (x', y') }
inputMotion _ state = return state


screenToPixel :: Point -> NCTState -> (Int, Int)
screenToPixel (x, y) (ChooseBase {
  imageScale = s,
  pictureSize = (px, py),
  screenSize = (sx, sy)
}) = (round ((x + (fromIntegral sx/2)) / s), round ((y + fromIntegral sy/2) / s))

screenToPixel (x, y) state = (round x, round y)
