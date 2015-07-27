module Render ( render
              ) where
import Control.Lens
import Data.Monoid

import Graphics.Gloss

import Types

render :: WorldState -> Picture
render state = if state^.gameOver
                 then gameOverScreen state
                 else color white $ box <> renderEntities state <> renderScore state

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

renderScore :: WorldState -> Picture
renderScore state = translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 . color white $ text (show $ state^.score)

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

gameOverScreen :: WorldState -> Picture
gameOverScreen state = gameOverText <> scoreText
  where
    gameOverText = scale 0.5 0.5 . translate (-resolution_w/2) 0 . color red $ text "Game Over"
    scoreText = scale 0.3 0.3 . translate 0 (-200) . color red $ text (show (state ^. score))
