module Render ( render
              , renderMainMenu
              ) where
import Control.Lens
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

getTextWidth :: Num a => String -> a
getTextWidth = fromIntegral . unsafePerformIO . GLUT.stringWidth GLUT.Roman

fontHeight :: Fractional a => a
fontHeight = realToFrac . unsafePerformIO $ GLUT.fontHeight GLUT.Roman

-- Testing
instance (Monoid a) => Monoid (Behavior a) where
  a `mappend` b = a >>= \a' -> (a' <>) <$> b
  mempty = return mempty
