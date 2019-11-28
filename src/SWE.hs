{-# LINE 1 "src/SWE.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module SWE where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- from: https://github.com/wkoszek/book-real-world-haskell/blob/master/examples/ch17/RegexFull.hsc
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe   as S
import qualified Data.ByteString.Internal as S



newtype PlanetNumber = PlanetNumber
  { unPlanetNumber :: CInt } deriving (Eq, Show)

newtype GregFlag = GregFlag
  { unGregFlag :: CInt } deriving (Eq, Show)

newtype CalcFlag = CalcFlag
  { unCalcFlag :: CInt } deriving (Eq, Show)

-- following:
-- https://en.wikibooks.org/wiki/Haskell/FFI#Enumerations

sun   :: PlanetNumber
sun   = PlanetNumber 0
moon  :: PlanetNumber
moon  = PlanetNumber 1
mercury  :: PlanetNumber
mercury  = PlanetNumber 2
venus  :: PlanetNumber
venus  = PlanetNumber 3
mars  :: PlanetNumber
mars  = PlanetNumber 4
jupiter  :: PlanetNumber
jupiter  = PlanetNumber 5
saturn  :: PlanetNumber
saturn  = PlanetNumber 6
uranus  :: PlanetNumber
uranus  = PlanetNumber 7
neptune  :: PlanetNumber
neptune  = PlanetNumber 8
pluto  :: PlanetNumber
pluto  = PlanetNumber 9
meanNode     :: PlanetNumber
meanNode     = PlanetNumber 10
trueNode  :: PlanetNumber
trueNode  = PlanetNumber 11
meanApog  :: PlanetNumber
meanApog  = PlanetNumber 12
oscuApog  :: PlanetNumber
oscuApog  = PlanetNumber 13
earth     :: PlanetNumber
earth     = PlanetNumber 14
chiron  :: PlanetNumber
chiron  = PlanetNumber 15

{-# LINE 46 "src/SWE.hsc" #-}

julian  :: GregFlag
julian  = GregFlag 0
gregorian  :: GregFlag
gregorian  = GregFlag 1

{-# LINE 51 "src/SWE.hsc" #-}

speed  :: CalcFlag
speed  = CalcFlag 256

{-# LINE 55 "src/SWE.hsc" #-}

-- functions to make the "mini" example work:

foreign import ccall unsafe "swephexp.h swe_set_ephe_path"
    c_swe_set_ephe_path :: CString -> IO ()


foreign import ccall unsafe "swephexp.h swe_julday"
    c_swe_julday :: CInt -- year
                 -> CInt -- month
                 -> CInt -- day 
                 -> CDouble -- hour
                 -> GregFlag
                 -> CDouble

foreign import ccall unsafe "swephexp.h swe_calc_ut"
    c_swe_calc :: CDouble
               -> PlanetNumber
               -> CalcFlag
               -> Ptr CDouble
               -> CString
               -> CalcFlag

foreign import ccall unsafe "swephexp.h swe_get_planet_name"
    c_swe_get_planet_name :: CInt
                          -> CString
                          -> CString

-- the glue

data Planet = Sun
            | Moon
            | Mercury
            | Venus
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            | Pluto
            | MeanNode
            | TrueNode
            | MeanApog
            | OscuApog
            | Earth
            | Chiron
            deriving (Show, Eq, Ord, Enum)

data Coordinates = Coords
  {
    lat :: Double
  , long :: Double
  , distance :: Double
  , longSpeed :: Double
  , latSpeed :: Double
  , distSpeed :: Double
  } deriving (Show, Eq, Ord)

planetNumber :: Planet -> PlanetNumber
planetNumber = PlanetNumber . fromEnum

setEphemeridesPath :: String -> IO ()
setEphemeridesPath path = unsafePerformIO $ do
  S.useAsCString path $ \ephePath -> do
    c_swe_set_ephe_path ephePath

julianDay :: Int -> Int -> Int -> Double -> Double
julianDay year month day hour = unsafePerformIO $ do
  realToFrac $ c_swe_julday y m d h gregorian
    where y = realToFrac year
          m = realToFrac month
          d = realToFrac day
          h = realToFrac hour

-- TODO: Planet Enum type
-- TODO: take an actual gregorian date!
calculateCoordinates :: Double -> Planet -> Either String [Double]
calculateCoordinates time planet = unsafePerformIO $ do
  allocaArray 6 $ \coords -> do
    alloca $ \error -> do
      iflgret <- c_swe_calc (realToFrac time)
                 planetNumber planet
                 speed
                 coords
                 error
      if (iflgret < 0)
        then do
          msg <- peekCString error
          return $ Left msg
        else do
          result <- peekArray coords -- TODO: wrong, needs to be some fancy type
          return $ Right result


-- a simple main:

-- https://www.astro.com/swisseph/swephprg.htm#_Toc19111155
