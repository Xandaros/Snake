{-|
Module: Types
-}
module Types ( resolution_w
             , resolution_h
             , Entity(..)
             , Snake
             , FoodPellet
             , Direction(..)
             ) where
import Prelude hiding (Either(Left, Right))

import Graphics.Gloss.Data.Point

-- | Datatype to represent the direction of the snake
data Direction = Up
               | Down
               | Left
               | Right
               deriving (Eq, Show)

-- | Datatype to represent an entity
data Entity = Entity { position :: Point
                     } deriving (Show)

-- | Typesynonym for the 'Snake', which consists of an 'Entity' for each segment
type Snake      = [Entity]
-- | Typesynonym for the 'FoodPellet', which is an 'Entity'
type FoodPellet = Entity

instance Eq Entity where
  (Entity a) == (Entity b) = a == b

{-# ANN module "HLint: ignore Use camelCase" #-}
-- | Resolution of the game
resolution_w, resolution_h :: Num a => a
resolution_w = 800
resolution_h = 600
