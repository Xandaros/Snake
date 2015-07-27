{-# LANGUAGE TemplateHaskell #-}
module Types ( resolution_w
             , resolution_h
             , initialWorldState
             , WorldState(..)
             , Entity(..)
             , Direction(..)
             , position
             , snakeSegments
             , foodPellet
             , direction
             , lastDirection
             , elapsedTime
             , lastMove
             , gameOver
             , score
             , rng
             , randomFoodPellet
             ) where
import Prelude hiding ( Either(Left, Right)
                      )
import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import System.Random

import Graphics.Gloss.Data.Point

data Direction = Up
               | Down
               | Left
               | Right
               deriving (Eq)

declareLenses [d|
    data Entity = Entity { position :: Point
                         }

    data WorldState = WorldState { snakeSegments :: ![Entity]
                                 , foodPellet    :: Entity
                                 , direction     :: Direction
                                 , lastDirection :: Direction
                                 , elapsedTime   :: Float
                                 , lastMove      :: Float
                                 , gameOver      :: Bool
                                 , score         :: Int
                                 , rng           :: StdGen
                                 }
    |]

instance Eq Entity where
  (Entity a) == (Entity b) = a == b

resolution_w,resolution_h :: Num a => a
resolution_w = 800
resolution_h = 600

randomFoodPellet :: State WorldState ()
randomFoodPellet = do
  emptyCells' <- emptyCells
  (randomCell, newGen) <- randomR (0, length emptyCells'-1) <$> use rng
  rng .= newGen
  foodPellet .= Entity (emptyCells' !! randomCell)
  where
    emptyCells :: State WorldState [Point]
    emptyCells = do
      segments <- use snakeSegments
      return [(x,y) | x <- [-(resolution_w/40-1)..(resolution_w/40-1)], y <- [-(resolution_h/40-1)..(resolution_h/40-1)], Entity (x,y) `notElem` segments]

initialWorldState :: IO WorldState
initialWorldState = execState randomFoodPellet <$> incompleteState
  where
    incompleteState = WorldState snake undefined Up Up 0 0 False 0 <$> getStdGen
    snake = reverse $ map Entity [ (-2,0)
                                 , (-1,0)
                                 , (0 ,0)
                                 , (1, 0)
                                 , (2, 0)
                                 ]
