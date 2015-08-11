{-|
Module: Util
Description: Utility functions
-}
module Util ( eventToKey
            , keyToDirection
            , isKeyDownEvent
            , waitForKey
            , filterKeys
            , whileKeyDown
            , keyToggle
            ) where
import Prelude hiding ( Either (Left, Right)
                      )

import Control.Monad
import Control.FRPNow
import Control.FRPNow.Gloss
import Graphics.Gloss.Interface.IO.Game hiding ( KeyState (Up, Down)
                                               , Event()
                                               )

import qualified Graphics.Gloss.Interface.IO.Game as KeyState ( KeyState (..)
                                                              )

import Types

-- | Turns a 'GEvent' into a 'Key', if it is a 'EventKey'
eventToKey :: GEvent    -- ^ The event
           -> Maybe Key
eventToKey (EventKey key _ _ _) = Just key
eventToKey _                    = Nothing

-- | Returns whether the giving event is an event describing a key being pressed
isKeyDownEvent :: GEvent -- ^ The event
               -> Bool
isKeyDownEvent (EventKey _ KeyState.Down _ _) = True
isKeyDownEvent _                              = False

-- | Turns a 'Key' into a 'Direction', if the key is a movement key
keyToDirection :: Key -- ^ The key
               -> Maybe Direction
keyToDirection (SpecialKey key) = case key of
  KeyLeft  -> Just Left
  KeyRight -> Just Right
  KeyUp    -> Just Up
  KeyDown  -> Just Down
  _        -> Nothing
keyToDirection _                = Nothing

-- | Generate an 'Event' which gets fired once the giving key is being pressed/released
waitForKey :: EvStream GEvent         -- ^ Global event stream
           -> Key                     -- ^ Key to wait for
           -> Maybe KeyState.KeyState -- ^ Whether to wait for press/release ('Just' 'KeyState.KeyState') or both ('Nothing')
           -> Behavior (Event ())
waitForKey evs key ks = do
  let keyEvents    = filterEs (isKeyEvent key) evs
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

-- | Given a stream of 'GEvent's, return a stream of 'GEvent's consisting of only events concerning the given 'Key's
filterKeys :: EvStream GEvent -- ^ Input event stream
           -> [Key]           -- ^ Keys to filter
           -> EvStream GEvent
filterKeys stream keys = do
  let keyEvents = filterEs isKeyEvent stream
  filterEs ((`elem` keys) . getKey) keyEvents
  where
    getKey :: GEvent -> Key -- PARTIAL!
    getKey (EventKey key _ _ _) = key

    isKeyEvent :: GEvent -> Bool
    isKeyEvent EventKey{} = True
    isKeyEvent _          = False

-- | Switch behaviour depending on whether a key is being held down
whileKeyDown :: EvStream GEvent -- ^ Event stream
             -> Key             -- ^ Key to switch on
             -> Behavior a      -- ^ 'Behavior' when the key not being held
             -> Behavior a      -- ^ 'Behavior' when the key is being held
             -> Behavior (Behavior a)
whileKeyDown evs key releasedBeh pressedBeh = do
  let keyEvs  = filterKeys evs [key]
      changes = fmap (\ev -> case ev of
                        (EventKey _ KeyState.Up   _ _) -> releasedBeh
                        (EventKey _ KeyState.Down _ _) -> pressedBeh
                     ) keyEvs
  releasedBeh `foldrSwitch` changes

-- | Toggle between two 'Behavior's when a given key is pressed
keyToggle :: EvStream GEvent -- ^ Event stream
          -> Key             -- ^ Key to toggle on
          -> Behavior a      -- ^ Initial behavior
          -> Behavior a      -- ^ Behavior to toggle between
          -> Behavior (Behavior a)
keyToggle evs key beh1 beh2 = do
  let keyEvs   = filterKeys evs [key]
      pressEvs = filterEs (\ev -> case ev of
                             (EventKey _ KeyState.Down _ _) -> True
                             _                              -> False
                          ) keyEvs
  behaviors <- scanlEv (\(_, b) _ -> if b
                                     then (beh1, False)
                                     else (beh2, True)
                       ) (undefined, False) pressEvs
  foldrSwitch beh1 (fst <$> behaviors)
