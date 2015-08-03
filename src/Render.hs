module Render ( render
              ) where
import Control.Lens
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

import Control.FRPNow
import Graphics.Gloss
import qualified Graphics.UI.GLUT as GLUT

import Types

render :: Behavior Snake -> Behavior FoodPellet -> Behavior Picture
render snake pellet = color white <$> renderSnake snake <> renderPellet pellet

renderSnake :: Behavior Snake -> Behavior Picture
renderSnake snake = snake >>= foldMap renderSegment
  where
    renderSegment :: Entity -> Behavior Picture
    renderSegment (Entity (x,y)) = return . translate (x*20) (y*20) . color white $ circleSolid 10

renderPellet :: Behavior FoodPellet -> Behavior Picture
renderPellet pellet = do
  (Entity (x,y)) <- pellet
  return . translate (x*20) (y*20) . color white $ circleSolid 7

--render :: Behavior WorldState -> Behavior Picture
--render state = do
--  state' <- state
--  case state'^.gameState of
--    GameOver -> gameOverScreen (view score <$> state)
--    Playing  -> renderPlaying
--    Paused   -> renderPlaying <> return (color (withAlpha 0.9 black) fillScreen)
--    MainMenu -> return . color white . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text startText
--    where
--      renderPlaying :: Behavior Picture
--      renderPlaying = color white <$> return box <> (renderEntities <$> state) <> (renderScore <$> state)
--      startText = "Press Enter to start"
--      textWidth = getTextWidth startText
--
--fillScreen :: Picture
--fillScreen = polygon [ (-w, -h)
--                   , (-w, h)
--                   , (w, h)
--                   , (w, -h)
--                   , (-w, -h)
--                   ]
--  where
--    w = resolution_w/2
--    h = resolution_h/2
--
--renderEntities :: WorldState -> Picture
--renderEntities state = renderSnake state <> renderFoodPellet state
--
--renderSnake :: WorldState -> Picture
--renderSnake state = foldMap renderSegment (state^.snakeSegments)
--  where
--    renderSegment :: Entity -> Picture
--    renderSegment ent@(Entity (x,y)) = translate (x*20) (y*20) . color white $ circleSolid 10
--
--renderFoodPellet :: WorldState -> Picture
--renderFoodPellet state = let Entity (x,y) = state^.foodPellet
--                         in translate (x*20) (y*20) . color white $ circleSolid 7
--
--renderScore :: WorldState -> Picture
--renderScore state = translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 . color white $ text (show $ state^.score)
--
--box :: Picture
--box = line [ ((-w)+10,(-h)+10)
--           , (w-10   ,(-h)+10)
--           , (w-10   ,h-10)
--           , ((-w)+10,h-10)
--           , ((-w)+10,(-h)+10)
--           ]
--  where
--    w = resolution_w/2
--    h = resolution_h/2
--
--gameOverScreen :: Behavior Int -> Behavior Picture
--gameOverScreen score = translate 0 (fontHeight/4-15) <$> (return gameOverText <> scoreText)
--  where
--    gameOverText = scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) . color red $ text endText
--    scoreText = do
--      scoreT <- show <$> score
--      return $ scale 0.5 0.5 . translate (-getTextWidth scoreT/2) (-(fontHeight + 30)) . color red $ text scoreT
--    endText = "Game Over"
--    textWidth = getTextWidth endText
--
----- Utility functions
--getTextWidth :: Num a => String -> a
--getTextWidth = fromIntegral . unsafePerformIO . GLUT.stringWidth GLUT.Roman
--
--fontHeight :: Fractional a => a
--fontHeight = realToFrac . unsafePerformIO $ GLUT.fontHeight GLUT.Roman

-- Testing
instance (Monoid a) => Monoid (Behavior a) where
  a `mappend` b = a >>= \a' -> (a' <>) <$> b
  mempty = return mempty
