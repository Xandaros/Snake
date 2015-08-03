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
  moveEvs <- sampleNow $ (const () <$>) <$> moveEvents time
  dir <- sampleNow $ getDirection events
  snake <- sampleNow (segments dir moveEvs)
  rng <- sync getStdGen
  pellet <- sampleNow . unloopify time $ foodPellet ((position <$>) <$> snake) (40, 30) foodEvs rng
  let foodEvs = foodEvents moveEvs snake pellet
  return $ render snake pellet

unloopify :: Behavior Time -> Behavior (Behavior a) -> Behavior (Behavior a)
unloopify time = join . (unloopify' <$>)
  where unloopify' beh = do
          beh' <- beh
          delay time beh' beh

--mainTest :: Behavior Float -> EvStream GEvent -> Now (Behavior Picture)
--mainTest f _ = do
--  asd <- sampleNow (update f)
--  traceChanges "" asd
--  return $ return blank
--
--update :: Behavior Float -> Behavior (Behavior Float)
--update time = foldB foldfunc 0 time
--  where
--    foldfunc :: Float -> Float -> Float
--    foldfunc i t = if t > i + 1 then i + 1 else i

--main :: IO ()
--main = initialWorldState >>= \initState -> play windowed black fps initState render inputHandler update
--  where
--    windowed = InWindow "Snake" (resolution_w,resolution_h) (0,0)
--    fullscreened = FullScreen (resolution_w, resolution_h)
