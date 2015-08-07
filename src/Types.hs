module Types ( resolution_w
             , resolution_h
             , Entity(..)
             , Snake
             , FoodPellet
             , Direction(..)
             , GameState(..)
             ) where
import Prelude hiding ( Either(Left, Right)
                      )
import Control.Monad.State
import System.Random

import Graphics.Gloss.Data.Point

data Direction = Up
               | Down
               | Left
               | Right
               deriving (Eq, Show)

data Entity = Entity { position :: Point
                     } deriving (Show)

data GameState = MainMenu
               | Playing
               | Paused
               | GameOver
               deriving (Eq, Show)

type Snake = [Entity]
type FoodPellet = Entity

instance Eq Entity where
  (Entity a) == (Entity b) = a == b

{-# ANN module "HLint: ignore Use camelCase" #-}
resolution_w,resolution_h :: Num a => a
resolution_w = 800
resolution_h = 600
