{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module SWE where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- from: https://github.com/wkoszek/book-real-world-haskell/blob/master/examples/ch17/RegexFull.hsc
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe   as S
import qualified Data.ByteString.Internal as S

#include <swephexp.h>

newtype PlanetNumber = PlanetNumber
  { unPlanetNumber :: CInt } deriving (Eq, Show)

newtype GregFlag = GregFlag
  { unGregFlag :: CInt } deriving (Eq, Show)

newtype CalcFlag = CalcFlag
  { unCalcFlag :: CInt } deriving (Eq, Show)

-- following:
-- https://en.wikibooks.org/wiki/Haskell/FFI#Enumerations

#{enum PlanetNumber, PlanetNumber,
  sun  = SE_SUN
 , moon = SE_MOON
 , mercury = SE_MERCURY
 , venus = SE_VENUS
 , mars = SE_MARS
 , jupiter = SE_JUPITER
 , saturn = SE_SATURN
 , uranus = SE_URANUS
 , neptune = SE_NEPTUNE
 , pluto = SE_PLUTO
 , meanNode    = SE_MEAN_NODE
 , trueNode = SE_TRUE_NODE
 , meanApog = SE_MEAN_APOG
 , oscuApog = SE_OSCU_APOG
 , earth    = SE_EARTH
 , chiron = SE_CHIRON
 }

#{enum GregFlag, GregFlag
 , julian = SE_JUL_CAL
 , gregorian = SE_GREG_CAL
 }

#{enum CalcFlag, CalcFlag
 , speed = SEFLG_SPEED
 }

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
