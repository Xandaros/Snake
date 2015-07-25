module Logic where
import Prelude hiding ( Either(Left, Right)
                      )

import Control.Lens
import Control.Monad.State
import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding ( Up, Down
                                                 )
import qualified Graphics.Gloss.Interface.Pure.Game as KeyState (KeyState (Up, Down)) 

import Types

update :: Float -> WorldState -> WorldState
update dt state = execState (update' dt) state

update' :: Float -> State WorldState ()
update' dt = do
  elapsedTime += dt
  elapsed <- use elapsedTime
  lm <- use lastMove
  when ((elapsed-lm) > 0.3) $ do
    lastMove .= elapsed
    makeMove
  return ()

makeMove :: State WorldState ()
makeMove = do
  (Entity firstSegment) <- snakeSegments `uses` head
  dir <- use direction
  let newSegment = newPos firstSegment dir
  snakeSegments %= (Entity newSegment:) . init
  where
    newPos (x,y) dir = case dir of
      Up    -> (x,y+1)
      Down  -> (x,y-1)
      Left  -> (x-1,y)
      Right -> (x+1, y)

inputHandler :: Event -> WorldState -> WorldState
inputHandler ev = execState (inputHandler' ev)

inputHandler' :: Event -> State WorldState ()
inputHandler' (EventKey (SpecialKey KeyLeft) KeyState.Down _ _)  = use direction >>= \dir -> unless (dir == Right) $ direction .= Left 
inputHandler' (EventKey (SpecialKey KeyRight) KeyState.Down _ _) = use direction >>= \dir -> unless (dir == Left ) $ direction .= Right 
inputHandler' (EventKey (SpecialKey KeyUp) KeyState.Down _ _)    = use direction >>= \dir -> unless (dir == Down ) $ direction .= Up    
inputHandler' (EventKey (SpecialKey KeyDown) KeyState.Down _ _)  = use direction >>= \dir -> unless (dir == Up   ) $ direction .= Down    
inputHandler' _ = return ()
