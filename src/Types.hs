module Types ( resolution_w
             , resolution_h
             , Entity(..)
             , Snake
             , FoodPellet
             , Direction(..)
             ) where
import Prelude hiding (Either(Left, Right))

import Graphics.Gloss.Data.Point

data Direction = Up
               | Down
               | Left
               | Right
               deriving (Eq, Show)

data Entity = Entity { position :: Point
                     } deriving (Show)

type Snake      = [Entity]
type FoodPellet = Entity

instance Eq Entity where
  (Entity a) == (Entity b) = a == b

{-# ANN module "HLint: ignore Use camelCase" #-}
resolution_w, resolution_h :: Num a => a
resolution_w = 800
resolution_h = 600
