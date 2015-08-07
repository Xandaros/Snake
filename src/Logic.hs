module Logic  where
import Prelude hiding ( Either(Left, Right)
                      )

import Control.Applicative
import Control.FRPNow
import Control.FRPNow.Gloss
import Control.Lens
import Control.Monad.State
import Data.Monoid
import System.Random (randomR, StdGen())

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding ( Up, Down, Event(..)
                                                 )
import qualified Graphics.Gloss.Interface.Pure.Game as KeyState (KeyState (Up, Down)) 

import Types
import Util

speed :: Behavior Time
speed = pure 1

moveEvents :: Behavior Time -> Behavior (EvStream Time)
moveEvents time = do
  currentTime <- time
  toChanges <$> foldB (\steps contin ->
                         if contin >= steps + 0.3
                         then steps + 0.3
                         else steps) currentTime time

segments :: Behavior Direction -> EvStream () -> EvStream () -> Behavior (Behavior Snake)
segments direction steps foodEvs = do
  afterFoodEvs <- foldEs foodFold snake foodEvs
  let dirs = snapshots direction steps
  foldBs afterFoodEvs moveFold dirs
  where
    moveFold :: Behavior Snake -> Direction -> Behavior Snake
    moveFold snake dir = makeMove <$> snake <*> pure dir
    foodFold :: Snake -> () -> Snake
    foodFold snake _ = snake ++ [last snake]
    snake = reverse $ map Entity [ (-2,0)
                                 , (-1,0)
                                 , (0 ,0)
                                 , (1 ,0)
                                 , (2 ,0)
                                 ]

playing :: EvStream GEvent -> Behavior GameState -> EvStream ()
playing events curState = do
  let keyStream = filterMapEs eventToKey events
      condition :: Behavior GameState -> Behavior (Key -> Bool)
      condition state = do
        state' <- state
        return (\key -> state' == MainMenu && key == SpecialKey KeyEnter)
  void $ filterB (condition curState) keyStream

-- FIXME: Delayed by one tick
gameOverEvent :: EvStream () -> Behavior Snake -> Behavior (Event ())
gameOverEvent evs snake = next . void $ filterEs (liftA2 (||) isOOB isIntersecting) (snapshots snake evs)
  where
    isOOB snake = let (Entity (x,y)) = head snake
                  in  x < -w || x > w || y < -h || y > h
    isIntersecting snake = let snakeHead = head snake
                           in  count snake (==snakeHead) >= 2
    w = resolution_w/40 - 1
    h = resolution_h/40 - 1

count :: [a] -> (a -> Bool) -> Int
count [] _ = 0
count (x:xs) f = if f x then 1 + count xs f else count xs f

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

initialDirection :: Direction
initialDirection = Up

lastDirection :: EvStream () -> Behavior Direction -> Behavior (Behavior Direction)
lastDirection evs dir = fromChanges initialDirection $ snapshots dir evs

getDirection :: Behavior Direction -> EvStream GEvent -> Behavior (Behavior Direction)
getDirection lastDir evs =
  let keyStream = filterMapEs (eventToKey >=> keyToDirection) evs
      filteredKeyStream = filterMapEsB (filterLastDir lastDir) keyStream
  in  fromChanges initialDirection filteredKeyStream
  where
    filterLastDir :: Behavior Direction -> Behavior (Direction -> Maybe Direction)
    filterLastDir lastDir = do
      lastDir' <- lastDir
      return (\dir -> if lastDir' == opposite dir then Nothing else Just dir)

    opposite :: Direction -> Direction
    opposite Left = Right
    opposite Right = Left
    opposite Up = Down
    opposite Down = Up

foodEvents :: EvStream () -> Behavior Snake -> Behavior FoodPellet -> EvStream ()
foodEvents evs snake pellet = do
  let snakePellets    = ((,) <$> pellet) <@@> snapshots snake evs :: EvStream (FoodPellet, Snake)
      intersectStream = isEating <$> snakePellets :: EvStream Bool
  void $ filterEs id intersectStream
  where
    isEating :: (FoodPellet, Snake) -> Bool
    isEating (Entity pellet, snake) = let snakeHead = position . head $ snake
                                      in  pellet == snakeHead

score :: EvStream () -> Behavior (Behavior Integer)
score ev = foldEs (\a _ -> a + 1) 0 ev

foodPellet :: Behavior [Point] -> (Int, Int) -> EvStream () -> StdGen -> Behavior (Behavior FoodPellet)
foodPellet occupied (width, height) evs rng = do
  occ <- occupied
  let occStream        = snapshots occupied evs
      (firstV, firstG) = scanFunc (undefined, rng) occ
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
