{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import           Import
import           System.Environment
import           Text.Read          (readMaybe)

envWithDefault :: String -> String -> IO String
envWithDefault key defaultValue = do
  actualValue <- lookupEnv key
  case actualValue of
    Nothing -> pure defaultValue
    Just v  -> pure v

readEphePath :: String -> IO EphePath
readEphePath def = do
  str <- envWithDefault "EPHE_PATH" def
  pure $ EphePath str

readGoogleApiKey :: IO GoogleApiKey
readGoogleApiKey = do
  val <- lookupEnv "GOOGLE_API_KEY"
  case val of
    Nothing -> error "$GOOGLE_API_KEY is required"
    Just v  -> pure $ GoogleApiKey v

readPort :: String -> IO Port
readPort def = do
  prt <- envWithDefault "PORT" def
  case (readMaybe prt :: Maybe Int) of
    Nothing -> error "Unable to parse $PORT"
    Just p  -> pure $ Port p
