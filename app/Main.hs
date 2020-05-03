{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import qualified Paths_senex
import           Run         (startApp)
import           Util

main :: IO ()
main = do
  -- arg to readEphePath is a default, should be the appropriate directory
  -- TODO(luis) maybe bundle this with the exec and get it from `Paths_senex`?
  ephePath <- readEphePath "/Users/luis/code/senex/csrc/sweph_18"
  port     <- readPort "3030"
  -- accepts no defaults, and it *must* be provided, as `$GOOGLE_API_KEY`.
  gapiKey  <- readGoogleApiKey
  lo       <- logOptionsHandle stderr False
  withLogFunc lo $ \lf ->
    let env = App lf ephePath port gapiKey
    in  startApp env
