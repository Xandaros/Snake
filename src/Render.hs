module Render ( render
              ) where
import Control.Lens
import Data.Monoid

import Graphics.Gloss

import Types

render :: WorldState -> Picture
render state = color white $ box <> renderEntities state

renderEntities :: WorldState -> Picture
renderEntities state = renderSnake state

renderSnake :: WorldState -> Picture
renderSnake state = foldMap renderSegment (state^.snakeSegments)
  where
    renderSegment :: Entity -> Picture
    renderSegment ent@(Entity (x,y)) = translate (x*20) (y*20) . color white $ circleSolid 10

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
