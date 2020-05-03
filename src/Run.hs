{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (startApp) where

import           API                      (app)
import           Data.ByteString.Char8    (pack)
import           Import
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)
import           SWE                      (setEphemeridesPath)

confirmRunning :: RIO App ()
confirmRunning = do
  env <- ask
  let p = view portL env
  let pth = view ephePathL env
  logInfo $ "Running on port: " <> (displayShow p) <> " with ephemerides path: " <> (displayShow pth)

startApp :: App -> IO ()
startApp env = do
  setEphemeridesPath $ pack $ unEphePath $ appEphePath env
  runRIO env confirmRunning
  let port = unPort $ appPort env
      settings =
        setPort port
        defaultSettings
  runSettings settings =<< app
