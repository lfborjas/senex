{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.SWE where

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
 , swissEph = SEFLG_SWIEPH
 , equatorialPositions = SEFLG_EQUATORIAL
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

foreign import ccall unsafe "swephexp.h swe_houses"
    c_swe_houses :: CDouble -- in fact, a Julian day "Number"
                 -> CDouble -- Lat
                 -> CDouble -- Long
                 -> CInt -- house system (see .hs version of this file)
                 -> Ptr CDouble -- cusps, 13 doubles (or 37 in system G)
                 -> Ptr CDouble -- ascmc, 10 doubles
                 -> IO ()