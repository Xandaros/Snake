module Logic ( update
             , inputHandler
             ) where
import Prelude hiding ( Either(Left, Right)
                      )

import Control.Lens
import Control.Monad.State

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding ( Up, Down
                                                 )
import qualified Graphics.Gloss.Interface.Pure.Game as KeyState (KeyState (Up, Down)) 

import Types

update :: Float -> WorldState -> WorldState
update dt = execState (update' dt)

update' :: Float -> State WorldState ()
update' dt = munless (use gameOver) $ do
  elapsedTime += dt
  elapsed <- use elapsedTime
  lm <- use lastMove
  when ((elapsed-lm) > 0.3) $ do
    lastMove .= elapsed
    makeMove
  collision <- checkCollision
  oob <- checkOOB
  when (collision || oob) $
    gameOver .= True
  return ()

makeMove :: State WorldState ()
makeMove = do
  (Entity firstSegment) <- snakeSegments `uses` head
  dir <- use direction
  let newSegment = newPos firstSegment dir
  snakeSegments %= (Entity newSegment:) . init
  where
    newPos (x,y) dir = case dir of
      Up        -> (x,y+1)
      Down      -> (x,y-1)
      Left      -> (x-1,y)
      Right     -> (x+1, y)

inputHandler :: Event -> WorldState -> WorldState
inputHandler ev = execState (inputHandler' ev)

inputHandler' :: Event -> State WorldState ()
inputHandler' (EventKey (SpecialKey key) KeyState.Down _ _ ) = case key of
  KeyLeft  -> Left `opp` Right
  KeyRight -> Right `opp` Left
  KeyUp    -> Up `opp` Down
  KeyDown  -> Down `opp` Up
  _        -> return ()
  where
    opp :: Direction -> Direction -> State WorldState ()
    opp a b = munless (use direction <&> (==b)) $ direction .= a

  --KeyLeft  -> use direction >>= \dir -> unless (dir == Right) $ direction .= Left
  --KeyRight -> use direction >>= \dir -> unless (dir == Left)  $ direction .= Right
  --KeyUp    -> use direction >>= \dir -> unless (dir == Down)  $ direction .= Up
  --KeyDown  -> use direction >>= \dir -> unless (dir == Up)    $ direction .= Down
inputHandler' _ = return ()

checkCollision :: State WorldState Bool
checkCollision = do
  segments <- use snakeSegments
  let positions = map (view position) segments
  return $ count positions (==head positions) > 1

checkOOB :: State WorldState Bool
checkOOB = do
  Entity (x,y) <- snakeSegments `uses` head
  if x < (-w) || x > w || y < (-h) || y > h
    then return True
    else return False
  where
    w = resolution_w/40 - 1
    h = resolution_h/40 - 1

count :: [a] -> (a -> Bool) -> Int
count [] _ = 0
count (x:xs) f = if f x then 1 + count xs f else count xs f

mwhen :: Monad m => m Bool -> m () -> m ()
mwhen pred act = do
  pr <- pred
  when pr act

munless :: Monad m => m Bool -> m () -> m ()
munless pred act = do
  pr <- pred
  unless pr act
