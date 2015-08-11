{-|
Module: Logic
Description: Contains the game logic
-}
module Logic  where
import Prelude hiding (Either(Left, Right))

import Control.Applicative
import Control.Monad
import Control.FRPNow hiding (integrate)
import Control.FRPNow.Gloss
import System.Random (randomR, StdGen())

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event(), KeyState(..))

import Types
import Util

-- | Generates an 'EvStream' where each event is spaced at a set time interval
timeInterval :: Behavior Time -- ^ Global time
             -> Float         -- ^ Space between events
             -> Behavior (EvStream Time)
timeInterval time iv = do
  curTime <- time
  toChanges <$> foldB (\steps contin ->
                         if contin >= steps + iv
                         then steps + iv
                         else steps
                      ) curTime time

-- | Generates an 'EvStream' where each event causes the snake to move
moveEvents :: Behavior Time  -- ^ Global time
           -> Behavior Float -- ^ Speed modifier
           -> Behavior Bool  -- ^ Whether the game is paused
           -> Behavior (EvStream Integer)
moveEvents time speed paused = do
  moveStream <- toChanges <$> (fmap round <$> integrate time speed)
  return $ moveStream `during` (not <$> paused)

-- | Generates a 'Behavior' which denotes whether the game is paused
-- Switches between 'True' and 'False' whenever the \'p\' key is pressed
paused :: EvStream GEvent -- ^ Global event stream
       -> Behavior (Behavior Bool)
paused evs = keyToggle evs (Char 'p') (pure False) (pure True)

-- | Generates a 'Behavior' which denotes the current speed
-- Switches to '10' is the space bar is held down.
-- '5' otherwise
speed :: EvStream GEvent -> Behavior (Behavior Float)
speed evs = whileKeyDown evs (SpecialKey KeySpace) (pure 5) (pure 10)

-- | Generates a 'Behavior' representing all the segments of the snake
segments :: Behavior Direction -- ^ The direction the snake is moving in
         -> EvStream ()        -- ^ Event stream where each event causes the snake to move ('moveEvents')
         -> EvStream ()        -- ^ Event stream where each event causes the snake to grow longer ('foodEvents')
         -> Behavior (Behavior Snake)
segments direction steps foodEvs = do
  afterFoodEvs <- foldEs foodFold snake foodEvs
  let dirs = snapshots direction steps
  foldBs afterFoodEvs moveFold dirs
  where
    moveFold :: Behavior Snake -> Direction -> Behavior Snake
    moveFold snake dir = makeMove <$> snake <*> pure dir

    foodFold :: Snake -> () -> Snake
    foodFold snake _ = snake ++ [last snake]

    snake :: Snake
    snake = reverse $ map Entity [ (-2,0)
                                 , (-1,0)
                                 , (0 ,0)
                                 , (1 ,0)
                                 , (2 ,0)
                                 ]

-- | Generates an event that fires when the game is lost
gameOverEvent :: EvStream ()    -- ^ Event stream where each event causes a check of the game over conditions ('moveEvents')
              -> Behavior Snake -- ^ The snake
              -> Behavior (Event ())
gameOverEvent evs snake = next . void $ filterEs (liftA2 (||) isOOB isIntersecting) (snapshots snake evs)
  where
    isOOB :: Snake -> Bool
    isOOB snake = let (Entity (x,y)) = head snake
                  in  x < -w || x > w || y < -h || y > h

    isIntersecting :: Snake -> Bool
    isIntersecting snake = let snakeHead = head snake
                           in  count snake (==snakeHead) >= 2

    w, h :: Float
    w = resolution_w/40 - 1
    h = resolution_h/40 - 1

-- | Counts how many list entires fulfill a given predicate
count :: [a]         -- ^ List to count elements of
      -> (a -> Bool) -- ^ Predicate to check
      -> Int
count [] _ = 0
count (x:xs) f = if f x
                 then 1 + count xs f
                 else count xs f

-- | Calculate the snake after moving once in the given 'Direction'
makeMove :: Snake     -- ^ Old snake
         -> Direction -- ^ Direction
         -> Snake
makeMove current dir =
  let (Entity firstSegment) = head current
      newSegment            = newPos firstSegment dir
  in  (Entity newSegment:) . init $ current
  where
    newPos (x,y) dir = case dir of
      Up    -> (x  ,y+1)
      Down  -> (x  ,y-1)
      Left  -> (x-1,y  )
      Right -> (x+1,y  )

-- | The direction immediately after the game starts
initialDirection :: Direction
initialDirection = Up

-- | Generates a 'Behavior' which represents the last 'Direction' the snake moved in
lastDirection :: EvStream ()        -- ^ Event stream where each event causes the direction to update ('moveEvents')
              -> Behavior Direction -- ^ 'Behavior' representing the current direction ('getDirection')
              -> Behavior (Behavior Direction)
lastDirection evs dir = fromChanges initialDirection $ snapshots dir evs

-- | Generates a 'Behavior' representing the current direction of the snake
-- The snake is unable to go back on itself
getDirection :: Behavior Direction -- ^ The last direction the snake moved in ('lastDirection')
             -> EvStream GEvent    -- ^ Global event stream
             -> Behavior (Behavior Direction)
getDirection lastDir evs =
  let keyStream         = filterMapEs (eventToKey >=> keyToDirection) evs
      filteredKeyStream = filterMapEsB (filterLastDir lastDir) keyStream
  in  fromChanges initialDirection filteredKeyStream
  where
    filterLastDir :: Behavior Direction -> Behavior (Direction -> Maybe Direction)
    filterLastDir lastDir = do
      lastDir' <- lastDir
      return (\dir -> if lastDir' == opposite dir
                      then Nothing
                      else Just dir
             )

    opposite :: Direction -> Direction
    opposite Left = Right
    opposite Right = Left
    opposite Up = Down
    opposite Down = Up

-- | Generates an 'EvStream' where each event represents the snake eating a food pellet
foodEvents :: EvStream ()         -- ^ Event stream where each event causes a check for whether the snake is currently eating ('moveEvents')
           -> Behavior Snake      -- ^ The snake
           -> Behavior FoodPellet -- ^ The food pellet
           -> EvStream ()
foodEvents evs snake pellet = do
  let snakePellets    = ((,) <$> pellet) <@@> snapshots snake evs :: EvStream (FoodPellet, Snake)
      intersectStream = isEating <$> snakePellets :: EvStream Bool
  void $ filterEs id intersectStream
  where
    isEating :: (FoodPellet, Snake) -> Bool
    isEating (Entity pellet, snake) = let snakeHead = position . head $ snake
                                      in  pellet == snakeHead

-- | Calculates a running total of how many food pellets were eaten
score :: EvStream () -- ^ An event stream where each event increases the score by ^ ('foodEvents')
      -> Behavior (Behavior Integer)
score ev = foldEs (\a _ -> a + 1) 0 ev

-- | Generates a 'Behavior' which represents the current food pellet
foodPellet :: Behavior [Point] -- ^ A list of points that are unsuitable for generation of a food pellet ('snake')
           -> (Int, Int)       -- ^ Width and height of the playing field
           -> EvStream ()      -- ^ An event stream where each event causes the food pellet to move ('foodEvents')
           -> StdGen           -- ^ A random number generator
           -> Behavior (Behavior FoodPellet)
foodPellet occupied (width, height) evs rng = do
  occ <- occupied
  let occStream        = snapshots occupied evs
      (firstV, firstG) = scanFunc (undefined, rng) occ
  evs' <- scanlEv scanFunc (firstV, firstG) occStream
  fromChanges (Entity firstV) (Entity . fst <$> evs')
  where
    available :: [Point] -> [Point]
    available occ = [(x,y) | x <- [-w..w], y <- [-h..h], (x,y) `notElem` occ]

    w, h :: Float
    w             = fromIntegral width  / 2-1
    h             = fromIntegral height / 2-1

    scanFunc :: (Point, StdGen) -> [Point] -> (Point, StdGen)
    scanFunc (_, gen) occ = let avail          = available occ
                                (rand, newGen) = randomR (0, length (available occ) - 1) gen
                            in  (avail !! rand, newGen)

-- | Integrate the given value over time
-- Thanks to <https://ocharles.org.uk/ Oliver Charles> for supplying this lovely function
integrate :: Behavior Float -- ^ Global time
          -> Behavior Float -- ^ Value to integrate
          -> Behavior (Behavior Float)
integrate time v =
  do t  <- time
     vp <- delay time (t,0) (liftA2 (,) time v)
     foldB add 0 (liftA2 (,) vp time)
  where
    add total ((t1, v), t2) = total + ((t2 - t1) * v)
