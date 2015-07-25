module Main where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Logic
import Render
import Types

fps :: Int
fps = 30

main :: IO ()
main = play (InWindow "Snake" (resolution_w,resolution_h) (0,0)) black fps initialWorldState render inputHandler update
