module Util ( eventToKey
            , keyToDirection
            , isKeyDownEvent
            , waitForKey
            ) where
import Prelude hiding ( Either (Left, Right)
                      )

import Control.Monad
import Control.FRPNow
import Control.FRPNow.Gloss
import Graphics.Gloss.Interface.IO.Game hiding ( KeyState (Up, Down)
                                               , Event()
                                               )

import qualified Graphics.Gloss.Interface.IO.Game as KeyState ( KeyState (Down)
                                                              )

import Types

eventToKey :: GEvent -> Maybe Key
eventToKey (EventKey key _ _ _) = Just key
eventToKey _ = Nothing

isKeyDownEvent :: GEvent -> Bool
isKeyDownEvent (EventKey _ KeyState.Down _ _) = True
isKeyDownEvent _ = False

keyToDirection :: Key -> Maybe Direction
keyToDirection (SpecialKey key) = case key of
  KeyLeft  -> Just Left
  KeyRight -> Just Right
  KeyUp    -> Just Up
  KeyDown  -> Just Down
  _        -> Nothing
keyToDirection _ = Nothing

waitForKey :: EvStream GEvent -> Key -> Maybe KeyState.KeyState -> Behavior (Event ())
waitForKey evs key ks = do
  let keyEvents = filterEs (isKeyEvent key) evs
      wantedEvents = filterEs (\(EventKey _ state _ _) -> case ks of
                                 Nothing -> True
                                 Just ks' -> ks' == state
                              ) keyEvents
  next . void $ wantedEvents
  where
    isKeyEvent :: Key -> GEvent -> Bool
    isKeyEvent key (EventKey k _ _ _) | key == k  = True
                                        | otherwise = False
    isKeyEvent _ _ = False
