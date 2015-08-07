{-# LANGUAGE RecursiveDo #-}

module Main where
import Prelude hiding(Either(..))

import Control.FRPNow
import Control.FRPNow.Gloss
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Graphics.Gloss
import System.CPUTime
import System.Random (getStdGen)

import Logic
import Render
import Types
import Util

fps :: Int
fps = 30

main :: IO ()
main = runNowGloss (InWindow "Snake" (resolution_w, resolution_h) (0, 0)) black fps mainFRP 

mainFRP :: Behavior Time -> EvStream GEvent -> Now (Behavior Picture)
mainFRP time events = mainFRP' time events mainMenu
  where
    mainFRP' :: Behavior Time
             -> EvStream GEvent
             -> (Behavior Time -> EvStream GEvent -> State)
             -> Now (Behavior Picture)
    mainFRP' time events func = do
      (pic, nextFunc) <- unState (func time events) :: Now (Behavior Picture, Event (Behavior Time -> EvStream GEvent -> State))
      nextState <- planNow $ mainFRP' time events <$> nextFunc
      return $ pic `switch` nextState

mainMenu :: Behavior Time -> EvStream GEvent -> State
mainMenu time events = State $ do
  let nextScreens = fmap (const game) . filterEs isEnter . filterMapEs eventToKey $ events
  nextScreen <- sampleNow $ next nextScreens
  return (renderMainMenu, nextScreen)
  where
    isEnter :: Key -> Bool
    isEnter (SpecialKey KeyEnter) = True
    isEnter _                     = False

game :: Behavior Time -> EvStream GEvent -> State
game time events = State $ mdo
  moveEvs <- sampleNow $ void <$> moveEvents time
  lastDir <- sampleNow . unloopify time $ lastDirection moveEvs dir
  dir <- sampleNow $ getDirection lastDir events
  snake <- sampleNow . unloopify time $ segments dir moveEvs foodEvs
  rng <- sync getStdGen
  pellet <- sampleNow . unloopify time $ foodPellet ((position <$>) <$> snake) (40, 30) foodEvs rng
  let foodEvs = foodEvents moveEvs snake pellet
  state <- sampleNow . unloopify time $ gameState (playing events state) (gameOver moveEvs snake)
  return (render snake pellet, never)

unloopify :: Behavior Time -> Behavior (Behavior a) -> Behavior (Behavior a)
unloopify time = join . (unloopify' <$>)
  where unloopify' beh = do
          beh' <- beh
          delay time beh' beh

-- TODO: Need better name
data State = State { unState :: Now (Behavior Picture, Event (Behavior Time -> EvStream GEvent -> State))
                   }
