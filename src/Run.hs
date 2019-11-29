{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

-- WARNING: not currently using this, need to learn more about RIO
run :: RIO App ()
run = do
  -- TODO: get this from the App context
  logInfo "Loaded ephemerides"
