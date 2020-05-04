{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Types where

import           RIO

newtype EphePath = EphePath { unEphePath :: String } deriving (Show)
newtype Port     = Port { unPort :: Int } deriving (Show)
newtype GoogleApiKey = GoogleApiKey { unKey :: String } deriving (Show)

data App = App
  { appLogFunc      :: !LogFunc
  , appEphePath     :: !EphePath
  , appPort         :: !Port
  , appGoogleApiKey :: !GoogleApiKey
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class HasEphePath env where
  ephePathL :: Lens' env EphePath
instance HasEphePath App where
  ephePathL = lens appEphePath (\x y -> x { appEphePath = y})

class HasPort env where
  portL :: Lens' env Port
instance HasPort App where
  portL = lens appPort (\x y -> x { appPort = y})

class HasGoogleApiKey env where
  googleApiKeyL :: Lens' env GoogleApiKey
instance HasGoogleApiKey App where
  googleApiKeyL = lens appGoogleApiKey (\x y -> x { appGoogleApiKey = y})
