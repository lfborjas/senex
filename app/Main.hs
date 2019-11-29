{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Import
import SWE (setEphemeridesPath)
import API (app)
import RIO.Process
import Options.Applicative.Simple
import           Network.Wai.Handler.Warp (run)
import qualified Paths_senex
import Prelude (putStrLn)

main :: IO ()
main = do
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  putStrLn "Running on port 3030"
  run 3030 app
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

 