module Util ( eventToKey
            , keyToDirection
            , isKeyDownEvent
            ) where
import Prelude hiding ( Either (Left, Right)
                      )

import Control.FRPNow.Gloss
import Graphics.Gloss.Interface.IO.Game hiding ( KeyState (Up, Down)
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
