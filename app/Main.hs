{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Import
import SWE (setEphemeridesPath)
import API (app)
import RIO.Process
import Options.Applicative.Simple
import           Network.Wai.Handler.Warp (runSettings, setPort, setBeforeMainLoop, defaultSettings)
import qualified Paths_senex
import Prelude (putStrLn)

main :: IO ()
main = do
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  -- from https://github.com/haskell-servant/servant-swagger/issues/45
  -- and https://gist.github.com/fizruk/f9213e20ea68a9d6128f515c70a6e0e3
  let port = 3030
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn $ "Listening on port " `mappend` show port)
        defaultSettings
  runSettings settings =<< app
  -- TODO : figure out how to use the RIO App monad to send context to 
  -- things...
  -- reference: https://github.com/gvolpe/exchange-rates/blob/master/src/Http/Server.hs
  {- (options, ()) <- simpleOptions
    $(simpleVersion Paths_senex.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run -}

 