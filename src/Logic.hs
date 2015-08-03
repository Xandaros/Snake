module Logic  where
import Prelude hiding ( Either(Left, Right)
                      )

import Control.FRPNow
import Control.FRPNow.Gloss
import Control.Lens
import Control.Monad.State
import Data.Maybe (fromJust)
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding ( Up, Down
                                                 )
import qualified Graphics.Gloss.Interface.Pure.Game as KeyState (KeyState (Up, Down)) 

import Types

--update' :: Behavior Float -> WorldState -> Behavior WorldState
--update' dt ws = mwhen (use gameState <&> (==Playing)) $ do
--  -- update time
--  elapsedTime += dt
--  elapsed <- use elapsedTime
--  -- check if enough time has passed to move the snake
--  lm <- use lastMove
--  spd <- use speed
--  spaceP <- use spacePressed
--  let spd' = if spaceP then spd/2 else spd
--  when ((elapsed-lm) > spd') $ do
--    lastMove .= elapsed
--    lastDirection <~ use direction
--    makeMove
--    -- check for food pickup
--    mwhen onFood $ do
--        snakeSegments %= \segments -> segments ++ [last segments]
--        score += 1
--        randomFoodPellet
--  -- check for game over
--  collision <- checkCollision
--  oob <- checkOOB
--  when (collision || oob) $
--    gameState .= GameOver

speed :: Behavior Time
speed = pure 1

moveEvents :: Behavior Time -> Behavior (EvStream Time)
moveEvents time =
  toChanges <$> foldB (\steps contin ->
                         if contin >= steps + 1
                         then steps + 1
                         else steps) 0 time

segments :: Behavior Direction -> EvStream () -> Behavior (Behavior Snake)
segments direction steps = do
  let dirs = snapshots direction steps
  foldBs (pure snake) foldFunc dirs
  where
    foldFunc :: Behavior Snake -> Direction -> Behavior Snake
    foldFunc snake dir = makeMove <$> snake <*> pure dir
    snake = reverse $ map Entity [ (-2,0)
                                 , (-1,0)
                                 , (0 ,0)
                                 , (1 ,0)
                                 , (2 ,0)
                                 ]

makeMove :: Snake -> Direction -> Snake
makeMove current dir =
  let (Entity firstSegment) = head current
      newSegment = newPos firstSegment dir
  in  (Entity newSegment:) . init $ current
  where
    newPos (x,y) dir = case dir of
      Up    -> (x  ,y+1)
      Down  -> (x  ,y-1)
      Left  -> (x-1,y  )
      Right -> (x+1,y  )

getDirection :: EvStream GEvent -> Behavior (Behavior Direction)
getDirection evs =
  let keyStream = filterMapEs (eventToKey >=> keyToDirection) evs
  in fromChanges Up keyStream
  where
    eventToKey :: GEvent -> Maybe Key
    eventToKey (EventKey key _ _ _) = Just key
    eventToKey _ = Nothing

    keyToDirection :: Key -> Maybe Direction
    keyToDirection (SpecialKey key) = case key of
      KeyLeft  -> Just Left
      KeyRight -> Just Right
      KeyUp    -> Just Up
      KeyDown  -> Just Down
      _        -> Nothing
    keyToDirection _ = Nothing

foodEvents :: EvStream () -> Behavior Snake -> Behavior FoodPellet -> EvStream ()
foodEvents evs snake pellet =
  let snakePellets    = ((,) <$> pellet) <@@> snapshots snake evs :: EvStream (FoodPellet, Snake)
      intersectStream = isEating <$> snakePellets :: EvStream Bool
  in void $ filterEs id intersectStream
  where
    isEating :: (FoodPellet, Snake) -> Bool
    isEating (Entity pellet, snake) = let snakeHead = position . head $ snake
                                      in  pellet == snakeHead

foodPellet :: Behavior [Point] -> (Int, Int) -> EvStream () -> StdGen -> Behavior (Behavior FoodPellet)
foodPellet occupied (width, height) evs rng = do
  occ <- occupied
  let occStream        = snapshots occupied evs
      (firstV, firstG) = scanFunc (undefined, firstG) occ
  evs' <- scanlEv scanFunc (firstV, firstG) occStream
  fromChanges (Entity firstV) (Entity . fst <$> evs')
  where
    available occ = [(x,y) | x <- [-w..w], y <- [-h..h], (x,y) `notElem` occ]
    w = fromIntegral width  / 2-1 :: Float
    h = fromIntegral height / 2-1 :: Float
    scanFunc :: (Point, StdGen) -> [Point] -> (Point, StdGen)
    scanFunc (_, gen) occ = let avail          = available occ
                                (rand, newGen) = randomR (0, length (available occ) - 1) gen
                            in  (avail !! rand, newGen)

--makeMove :: State WorldState ()
--makeMove = do
--  (Entity firstSegment) <- snakeSegments `uses` head
--  dir <- use direction
--  let newSegment = newPos firstSegment dir
--  snakeSegments %= (Entity newSegment:) . init
--  where
--    newPos (x,y) dir = case dir of
--      Up        -> (x,y+1)
--      Down      -> (x,y-1)
--      Left      -> (x-1,y)
--      Right     -> (x+1, y)
--
--inputHandler :: Event -> WorldState -> WorldState
--inputHandler ev = execState (inputHandler' ev)
--
--inputHandler' :: Event -> State WorldState ()
--inputHandler' (EventKey (SpecialKey key) keystate _ _) = do
--  state <- use gameState
--  case state of
--    Playing -> case key of
--        KeyLeft  -> when (keystate == KeyState.Down) $ Left  `opp` Right
--        KeyRight -> when (keystate == KeyState.Down) $ Right `opp` Left
--        KeyUp    -> when (keystate == KeyState.Down) $ Up    `opp` Down
--        KeyDown  -> when (keystate == KeyState.Down) $ Down  `opp` Up
--        KeySpace -> if keystate == KeyState.Down
--                    then spacePressed .= True
--                    else spacePressed .= False
--        _        -> return ()
--    MainMenu -> when (key == KeyEnter && keystate == KeyState.Down) $ gameState .= Playing
--    Paused   -> return ()
--    GameOver -> when (key == KeyEnter && keystate == KeyState.Down) $ do
--      rand <- use rng
--      put pureInitialWorldState
--      rng .= rand
--      randomFoodPellet
--  where
--    opp :: Direction -> Direction -> State WorldState ()
--    opp a b = munless (use lastDirection <&> (==b)) $ direction .= a
--inputHandler' (EventKey (Char 'p') KeyState.Down _ _) = do
--  state <- use gameState
--  case state of
--    Playing -> gameState .= Paused
--    Paused  -> gameState .= Playing
--    _       -> return ()
--inputHandler' _ = return ()
--
--checkCollision :: State WorldState Bool
--checkCollision = do
--  segments <- use snakeSegments
--  let positions = map (view position) segments
--  return $ count positions (==head positions) > 1
--checkOOB = do
--  Entity (x,y) <- snakeSegments `uses` head
--  if x < (-w) || x > w || y < (-h) || y > h
--    then return True
--    else return False
--  where
--    w = resolution_w/40 - 1
--    h = resolution_h/40 - 1
--
--onFood :: State WorldState Bool
--onFood = do
--  snakeHead <- use $ snakeSegments . singular _head . position
--  pellet <- use $ foodPellet . position
--  return (snakeHead == pellet)
--
--count :: [a] -> (a -> Bool) -> Int
--count [] _ = 0
--count (x:xs) f = if f x then 1 + count xs f else count xs f
--
--mwhen :: Monad m => m Bool -> m () -> m ()
--mwhen pred act = do
--  pr <- pred
--  when pr act
--
--munless :: Monad m => m Bool -> m () -> m ()
--munless pred act = do
--  pr <- pred
--  unless pr act
