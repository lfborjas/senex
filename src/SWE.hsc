{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module SWE where

import Foreign
import Foreign.C.Types

#include <swephexp.h>

newtype PlanetNumber = PlanetNumber
  { unPlanetNumber :: CInt } deriving (Eq, Show)

newtype GregFlag = GregFlag
  { unGregFlag :: CInt } deriving (Eq, Show)

-- following:
-- https://en.wikibooks.org/wiki/Haskell/FFI#Enumerations

#{enum PlanetNumber, PlanetNumber
 , sun  = SE_SUN
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

foreign import ccall unsafe "swephexp.h swe_calc"
c_swe_calc :: CDouble
           -> CInt
           -> ??
           -> Ptr CDouble
           -> Ptr CString
