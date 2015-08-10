module Render ( renderGame
              , renderMainMenu
              , renderGameOver
              ) where
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

import Control.FRPNow
import Graphics.Gloss
import qualified Graphics.UI.GLUT as GLUT

import Types

renderMainMenu :: Behavior Picture
renderMainMenu = return $ color white . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text startText
  where
    startText = "Press Enter to start"
    textWidth = getTextWidth startText

renderGameOver :: Integer -> Behavior Picture
renderGameOver score = return $ (color red . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text gameOverText)
                             <> (color red . scale 0.3 0.3 . translate (-scoreTextWidth/2) (-fontHeight/2*3) $ text scoreText)
  where
    gameOverText = "Game Over"
    textWidth = getTextWidth gameOverText
    scoreText = "Score: " ++ show score
    scoreTextWidth = getTextWidth scoreText

renderGame :: Behavior Snake -> Behavior FoodPellet -> Behavior Integer -> Behavior Picture
renderGame snake pellet score = color white <$> renderSnake snake <> renderPellet pellet <> renderScore score

renderSnake :: Behavior Snake -> Behavior Picture
renderSnake snake = snake >>= foldMap renderSegment
  where
    renderSegment :: Entity -> Behavior Picture
    renderSegment (Entity (x,y)) = return . translate (x*20) (y*20) . color white $ circleSolid 10

renderPellet :: Behavior FoodPellet -> Behavior Picture
renderPellet pellet = do
  (Entity (x,y)) <- pellet
  return . translate (x*20) (y*20) . color white $ circleSolid 7

renderScore :: Behavior Integer -> Behavior Picture
renderScore score = do
  score' <- score
  return . color white . translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 $ text (show score')
-- renderScore state = translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 . color white $ text (show $ state^.score)


getTextWidth :: Num a => String -> a
getTextWidth = fromIntegral . unsafePerformIO . GLUT.stringWidth GLUT.Roman

fontHeight :: Fractional a => a
fontHeight = realToFrac . unsafePerformIO $ GLUT.fontHeight GLUT.Roman

-- Testing
instance (Monoid a) => Monoid (Behavior a) where
  a `mappend` b = a >>= \a' -> (a' <>) <$> b
  mempty = return mempty
