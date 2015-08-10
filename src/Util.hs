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

filterKeys :: EvStream GEvent -> [Key] -> EvStream GEvent
filterKeys stream keys = do
  let keyEvents = filterEs isKeyEvent stream
  filterEs ((`elem` keys) . getKey) keyEvents
  where
    getKey :: GEvent -> Key -- PARTIAL!
    getKey (EventKey key _ _ _) = key

    isKeyEvent :: GEvent -> Bool
    isKeyEvent EventKey{} = True
    isKeyEvent _ = False

whileKeyDown :: EvStream GEvent -> Key -> Behavior a -> Behavior a -> Behavior (Behavior a)
whileKeyDown evs key releasedBeh pressedBeh = do
  let keyEvs = filterKeys evs [key]
      changes = fmap (\ev -> case ev of
                        (EventKey _ KeyState.Up _ _) -> releasedBeh
                        (EventKey _ KeyState.Down _ _) -> pressedBeh) keyEvs
  releasedBeh `foldrSwitch` changes

keyToggle :: EvStream GEvent -> Key -> Behavior a -> Behavior a -> Behavior (Behavior a)
keyToggle evs key beh1 beh2 = do
  let keyEvs = filterKeys evs [key]
      pressEvs = filterEs (\ev -> case ev of
                             (EventKey _ KeyState.Down _ _) -> True
                             _ -> False
                          ) keyEvs
  behaviors <- scanlEv (\(_, b) _ -> if b then (beh1, False) else (beh2, True)) (undefined, False) pressEvs
  foldrSwitch beh1 (fst <$> behaviors)
