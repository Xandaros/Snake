{-# LANGUAGE TemplateHaskell #-}
module Types ( resolution_w
             , resolution_h
             , initialWorldState
             , WorldState(..)
             , Entity(..)
             , Direction(..)
             , position
             , snakeSegments
             , foodPellets
             , direction
             , elapsedTime
             , lastMove
             , gameOver
             ) where
import Prelude hiding ( Either(Left, Right)
                      )

import Control.Lens
import Control.Lens.TH
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
                                 , foodPellets   :: [Entity]
                                 , direction     :: Direction
                                 , elapsedTime   :: Float
                                 , lastMove      :: Float
                                 , gameOver      :: Bool
                                 }
    |]

resolution_w,resolution_h :: Num a => a
resolution_w = 800
resolution_h = 600

initialWorldState :: WorldState
initialWorldState = WorldState snake [] Up 0 0 False
  where
    snake = reverse $ map Entity [ (-2,0)
                                 , (-1,0)
                                 , (0 ,0)
                                 , (1, 0)
                                 , (2, 0)
                                 ]
