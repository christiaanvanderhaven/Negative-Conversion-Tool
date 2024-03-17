module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> NCTState -> IO NCTState
step _ EmptyState = return EmptyState
step _ state = return state

input :: Event -> NCTState -> IO NCTState
input _ EmptyState = return EmptyState

input e@(EventKey _ _ _ _) state = inputKey e state

input _ state = return state


inputKey :: Event -> NCTState -> IO NCTState
inputKey (EventKey (Char '=') Down _ _) state@(ChooseBase { imageScale = s }) = return state {imageScale = s * 2}
inputKey (EventKey (Char '-') Down _ _) state@(ChooseBase { imageScale = s }) = return state {imageScale = s / 2}
inputKey _ state = return state
