module Main where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Logic
import Render
import Types

fps :: Int
fps = 30

main :: IO ()
main = initialWorldState >>= \initState -> play windowed black fps initState render inputHandler update
  where
    windowed = InWindow "Snake" (resolution_w,resolution_h) (0,0)
    fullscreened = FullScreen (resolution_w, resolution_h)
