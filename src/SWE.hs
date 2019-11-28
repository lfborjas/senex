module SWE where

import Foreign.SWE

import Foreign
import System.IO.Unsafe
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe   as S
import qualified Data.ByteString.Internal as S

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
planetNumber p = PlanetNumber x
    where x = CInt y
          y = fromIntegral $ fromEnum p :: Int32

setEphemeridesPath :: S.ByteString -> ()
setEphemeridesPath path = unsafePerformIO $ do
  S.useAsCString path $ \ephePath -> do
    c_swe_set_ephe_path ephePath

julianDay :: Int -> Int -> Int -> Double -> Double
julianDay year month day hour = 
    realToFrac $ do
        c_swe_julday y m d h gregorian
            where y = fromIntegral year
                  m = fromIntegral month
                  d = fromIntegral day
                  h = realToFrac hour

-- TODO: Planet Enum type
-- TODO: take an actual gregorian date!
calculateCoordinates :: Double -> Planet -> Either String [Double]
calculateCoordinates time planet = unsafePerformIO $ do
    allocaArray 6 $ \coords -> do
        alloca $ \error -> do
            let iflgret = c_swe_calc 
                            (realToFrac time)
                            (planetNumber planet)
                            speed
                            coords
                            error
            
            if (unCalcFlag iflgret < 0)
                then do
                    msg <- peekCString error
                    return $ Left msg
                else do
                    result <- peekArray 6 coords
                    return $ Right $ map realToFrac result


-- a simple main:

-- https://www.astro.com/swisseph/swephprg.htm#_Toc19111155