{-# LANGUAGE RecursiveDo #-}

module Main where
import Prelude hiding(Either(..))

import Control.FRPNow
import Control.FRPNow.Gloss
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Graphics.Gloss
import System.CPUTime
import System.Random

import Logic
import Render
import Types

fps :: Int
fps = 30

main :: IO ()
main = runNowGloss (InWindow "Snake" (resolution_w, resolution_h) (0, 0)) black fps mainFRP 

mainFRP :: Behavior Float -> EvStream GEvent -> Now (Behavior Picture)
mainFRP time events = mdo
  moveEvs <- sampleNow $ void <$> moveEvents time
  lastDir <- sampleNow . unloopify time $ lastDirection moveEvs dir
  dir <- sampleNow $ getDirection lastDir events
  snake <- sampleNow . unloopify time $ segments dir moveEvs foodEvs
  rng <- sync getStdGen
  pellet <- sampleNow . unloopify time $ foodPellet ((position <$>) <$> snake) (40, 30) foodEvs rng
  let foodEvs = foodEvents moveEvs snake pellet
  state <- sampleNow . unloopify time $ gameState (playing events state) (gameOver moveEvs snake)
  return $ render snake pellet

unloopify :: Behavior Time -> Behavior (Behavior a) -> Behavior (Behavior a)
unloopify time = join . (unloopify' <$>)
  where unloopify' beh = do
          beh' <- beh
          delay time beh' beh
