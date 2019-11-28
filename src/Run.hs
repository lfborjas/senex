{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import SWE

run :: RIO App ()
run = do
  -- from https://www.astro.com/swisseph/swephprg.htm#_Toc19111155
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  time <- julianDay 1989 1 6 0.0
  forM_ [Sun .. Chiron] $ \planet ->
    print planet ++ (calculateCoordinates time planet)
  
