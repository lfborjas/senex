{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_senex
import SWE
import           Prelude                        ( putStrLn )

main :: IO ()
main = do
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  let time = julianDay 1989 1 6 0.0
  let coords = map (\p -> (p, (calculateCoordinates time p))) [Sun .. Chiron]
  forM_ coords $ \(planet, coord)->
    putStrLn $ show planet ++ ": " ++ show coord
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

 