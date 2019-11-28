{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import SWE
import           Prelude                        ( putStrLn )

run :: RIO App ()
run =
  logInfo "Hi"

  {- run = do
  -- from https://www.astro.com/swisseph/swephprg.htm#_Toc19111155
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  let time = julianDay 1989 1 6 0.0
  forM_ [Sun .. Chiron] $ \planet -> do
    coords <- calculateCoordinates time planet
    return putStrLn $ (show planet) ++ (show coords) -}