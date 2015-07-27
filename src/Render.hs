module Render ( render
              ) where
import Control.Lens
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Gloss
import qualified Graphics.UI.GLUT as GLUT

import Types

render :: WorldState -> Picture
render state = case state^.gameState of
  GameOver -> gameOverScreen state
  Playing  -> color white $ box <> renderEntities state <> renderScore state
  MainMenu -> color white . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text startText
  where
    renderPlaying = color white $ box <> renderEntities state <> renderScore state
    startText = "Press Enter to start"
    textWidth = getTextWidth startText

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
gameOverScreen state = translate 0 (fontHeight/4-15) $ gameOverText <> scoreText
  where
    gameOverText = scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) . color red $ text endText
    scoreText = scale 0.5 0.5 . translate (-getTextWidth scoreT/2) (-(fontHeight + 30)) . color red $ text scoreT
      where
        scoreT = show (state^.score)
    endText = "Game Over"
    textWidth = getTextWidth endText

--- Utility functions
getTextWidth :: Num a => String -> a
getTextWidth = fromIntegral . unsafePerformIO . GLUT.stringWidth GLUT.Roman

fontHeight :: Fractional a => a
fontHeight = realToFrac . unsafePerformIO $ GLUT.fontHeight GLUT.Roman
