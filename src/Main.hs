{-# LANGUAGE RecursiveDo #-}
{-|
Module: Main
-}
module Main where
import Prelude hiding(Either(..))

import Control.FRPNow
import Control.FRPNow.Gloss
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import qualified Graphics.Gloss.Interface.Pure.Game as KeyState (KeyState(..))
import System.Random (getStdGen)

import Logic
import Render
import Types
import Util

-- | Target FPS
fps :: Int
fps = 30

-- | Program entry point
main :: IO ()
main = runNowGloss (InWindow "Snake" (resolution_w, resolution_h) (0, 0)) black fps mainFRP 

-- | Main function
-- Responsible for switching between menus and providing glue code between the different systems
mainFRP :: Behavior Time   -- ^ Global time
        -> EvStream GEvent -- ^ Global event stream
        -> Now (Behavior Picture)
mainFRP time events = mainFRP' time events mainMenu
  where
    mainFRP' :: Behavior Time
             -> EvStream GEvent
             -> (Behavior Time -> EvStream GEvent -> State)
             -> Now (Behavior Picture)
    mainFRP' time events func = do
      (pic, nextFunc) <- unState (func time events) :: Now (Behavior Picture, Event (Behavior Time -> EvStream GEvent -> State))
      nextState       <- planNow $ mainFRP' time events <$> nextFunc
      return $ pic `switch` nextState

-- | The main menu
-- Waits for the enter key and switches to 'game'
mainMenu :: Behavior Time   -- ^ Global time
         -> EvStream GEvent -- ^ Global event stream
         -> State
mainMenu _ events = State $ do
  keyEv <- sampleNow $ waitForKey events (SpecialKey KeyEnter) (Just KeyState.Down)
  return (renderMainMenu, const game <$> keyEv)

-- TODO: Ununloopify...
-- | The actual game
-- Contains glue code between the "Logic" and the "Render"ing
-- Switches to 'gameOver' after the game ends
game :: Behavior Time   -- ^ Global time
     -> EvStream GEvent -- ^ Global event stream
     -> State
game time events = State $ mdo
  curTime        <- sampleNow                  $ time
  delayedTime    <- sampleNow                  $ delay time curTime time
  speed'         <- sampleNow                  $ speed events
  paused'        <- sampleNow                  $ paused events
  moveEvs        <- sampleNow                  $ void <$> moveEvents time speed' paused'
  delayedMoveEvs <- sampleNow                  $ void <$> moveEvents delayedTime speed' paused'
  lastDir        <- sampleNow . unloopify time $ lastDirection moveEvs dir
  dir            <- sampleNow                  $ getDirection lastDir events
  snake          <- sampleNow . unloopify time $ segments dir moveEvs foodEvs
  rng            <- sync getStdGen
  pellet         <- sampleNow . unloopify time $ foodPellet ((position <$>) <$> snake) (40, 30) foodEvs rng
  let foodEvs     =                              foodEvents delayedMoveEvs snake pellet
  gameOverEv     <- sampleNow                  $ gameOverEvent delayedMoveEvs snake
  score'         <- sampleNow                  $ score foodEvs
  return (renderGame snake pellet score' paused', const (gameOver score') <$> gameOverEv)

-- | Game over screen
-- Waits for the enter key and switches to 'mainMenu'
gameOver :: Behavior Integer -- ^ Score to display
         -> Behavior Time    -- ^ Global time
         -> EvStream GEvent  -- ^ Global event stream
         -> State
gameOver score _ events = State $ do
  lastScore <- sampleNow score
  keyEv     <- sampleNow $ waitForKey events (SpecialKey KeyEnter) (Just KeyState.Down)
  return (renderGameOver lastScore, const mainMenu <$> keyEv)

-- | Introduce delay in the inner 'Behavior' to prevent immediate feedback loops
-- This is a hack and should be removed once <https://github.com/atzeus/FRPNow/issues/7 FRPNow Issue #7> is resolved
unloopify :: Behavior Time -> Behavior (Behavior a) -> Behavior (Behavior a)
unloopify time = join . (unloopify' <$>)
  where unloopify' beh = do
          beh' <- beh
          delay time beh' beh

-- TODO: Need better name
-- | Return value for menus
-- This is a simple box type to prevent functions from returning an infinite type
data State = State { unState :: Now (Behavior Picture, Event (Behavior Time -> EvStream GEvent -> State))
                   }
