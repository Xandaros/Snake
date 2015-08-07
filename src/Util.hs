module Util ( eventToKey
            , keyToDirection
            ) where
import Prelude hiding ( Either (Left, Right)
                      )

import Control.FRPNow.Gloss
import Graphics.Gloss.Interface.IO.Game hiding ( KeyState (Up, Down)
                                               )

import Types

eventToKey :: GEvent -> Maybe Key
eventToKey (EventKey key _ _ _) = Just key
eventToKey _ = Nothing

keyToDirection :: Key -> Maybe Direction
keyToDirection (SpecialKey key) = case key of
  KeyLeft  -> Just Left
  KeyRight -> Just Right
  KeyUp    -> Just Up
  KeyDown  -> Just Down
  _        -> Nothing
keyToDirection _ = Nothing
