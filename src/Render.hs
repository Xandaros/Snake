{-|
Module: Render
-}

module Render ( renderGame
              , renderMainMenu
              , renderGameOver
              ) where
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Control.FRPNow
import Graphics.Gloss
import qualified Graphics.UI.GLUT as GLUT

import Types

-- | Renders the main menu
renderMainMenu :: Behavior Picture
renderMainMenu = return $ color white . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text startText
  where
    startText = "Press Enter to start"
    textWidth = getTextWidth startText

-- | Renders the game over screen
renderGameOver :: Integer          -- ^ Score to be displayed
               -> Behavior Picture
renderGameOver score = return $ (color red . scale 0.5 0.5 . translate (-textWidth/2) (-fontHeight/4) $ text gameOverText)
                             <> (color red . scale 0.3 0.3 . translate (-scoreTextWidth/2) (-fontHeight/2*3) $ text scoreText)
  where
    gameOverText = "Game Over"
    textWidth = getTextWidth gameOverText
    scoreText = "Score: " ++ show score
    scoreTextWidth = getTextWidth scoreText

-- | Renders the current frame of the game
renderGame :: Behavior Snake      -- ^ Current Snake
           -> Behavior FoodPellet -- ^ Current FoodPellet
           -> Behavior Integer    -- ^ Current Score
           -> Behavior Bool       -- ^ Whether the game is currently paused
           -> Behavior Picture
renderGame snake pellet score paused = do
  let pauseOverlay = fmap (\b -> if b
                                 then pauseOverlayP
                                 else blank
                          ) paused
  color white <$> renderSnake snake <> pure box <> renderPellet pellet <> renderScore score <> pauseOverlay
  where
    pauseOverlayP :: Picture
    pauseOverlayP = color (withAlpha 0.9 black) fillScreen

-- | Renders the Snake Entity
renderSnake :: Behavior Snake   -- ^ Current Snake
            -> Behavior Picture
renderSnake snake = snake >>= foldMap renderSegment
  where
    renderSegment :: Entity -> Behavior Picture
    renderSegment (Entity (x,y)) = return . translate (x*20) (y*20) . color white $ circleSolid 10

-- | Render the FoodPellet Entity
renderPellet :: Behavior FoodPellet -- ^ Current FoodPellet
             -> Behavior Picture
renderPellet pellet = do
  (Entity (x,y)) <- pellet
  return . translate (x*20) (y*20) . color white $ circleSolid 7

-- | Render the current score in the top-left corner
renderScore :: Behavior Integer -- ^ Current score
            -> Behavior Picture
renderScore score = do
  score' <- score
  return . color white . translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 $ text (show score')
-- renderScore state = translate ((-resolution_w/2)+20) ((resolution_h/2)-40) . scale 0.2 0.2 . color white $ text (show $ state^.score)

-- | Clear the screen
fillScreen :: Picture
fillScreen = polygon [ (-w, -h)
                     , (-w, h)
                     , (w, h)
                     , (w, -h)
                     , (-w, -h)
                     ]
  where
    w, h :: Float
    w = resolution_w/2
    h = resolution_h/2

-- | Render a box around the screen with distance of 10px
box :: Picture
box = line [ ((-w)+10,(-h)+10)
           , (w-10   ,(-h)+10)
           , (w-10   ,h-10)
           , ((-w)+10,h-10)
           , ((-w)+10,(-h)+10)
           ]
  where
    w, h :: Float
    w = resolution_w/2
    h = resolution_h/2

-- | Get the width of a given text in units (whatever that is)
getTextWidth :: Num a => String -- ^ 'String' to get the width of
             -> a
getTextWidth = fromIntegral . unsafePerformIO . GLUT.stringWidth GLUT.Roman

-- | Height of the font used by gloss
fontHeight :: Fractional a => a
fontHeight = realToFrac . unsafePerformIO $ GLUT.fontHeight GLUT.Roman

-- Testing
instance (Monoid a) => Monoid (Behavior a) where
  a `mappend` b = a >>= \a' -> (a' <>) <$> b
  mempty = return mempty
