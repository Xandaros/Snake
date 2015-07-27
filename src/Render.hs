module Render ( render
              ) where
import Control.Lens
import Data.Monoid

import Graphics.Gloss

import Types

render :: WorldState -> Picture
render state = if state^.gameOver
                 then gameOverScreen
                 else color white $ box <> renderEntities state

renderEntities :: WorldState -> Picture
renderEntities state = renderSnake state <> renderFoodPellet state

renderSnake :: WorldState -> Picture
renderSnake state = foldMap renderSegment (state^.snakeSegments)
  where
    renderSegment :: Entity -> Picture
    renderSegment ent@(Entity (x,y)) = translate (x*20) (y*20) . color white $ circleSolid 10

renderFoodPellet :: WorldState -> Picture
renderFoodPellet state = let Entity (x,y) = state^.foodPellet
                         in translate (x*20) (y*20) . color white $ circleSolid 7

box :: Picture
box = line [ ((-w)+10,(-h)+10)
           , (w-10   ,(-h)+10)
           , (w-10   ,h-10)
           , ((-w)+10,h-10)
           , ((-w)+10,(-h)+10)
           ]
  where
    w = resolution_w/2
    h = resolution_h/2

gameOverScreen :: Picture
gameOverScreen = scale 0.5 0.5 . translate (-resolution_w/2) 0 . color red $ text "Game Over"
