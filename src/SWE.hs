{-# LANGUAGE DeriveGeneric     #-}

module SWE where

import           Foreign.SWE

import           Foreign
import           GHC.Generics
import           System.IO.Unsafe
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Char                      ( ord )
import qualified Data.ByteString.Char8         as S

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
            deriving (Show, Eq, Ord, Enum, Generic)

data HouseSystem = Placidus
                 | Koch
                 | Porphyrius
                 | Regiomontanus
                 | Campanus
                 | Equal
                 | WholeSign
                 deriving (Show, Eq, Ord, Generic)

data Coordinates = Coords
  {
    lat :: Double
  , long :: Double
  , distance :: Double
  , longSpeed :: Double
  , latSpeed :: Double
  , distSpeed :: Double
  } deriving (Show, Eq, Ord, Generic)

data HouseCusps = Cusps
  {
      i :: Double
    , ii :: Double
    , iii :: Double
    , iv :: Double
    , v :: Double
    , vi :: Double
    , vii :: Double
    , viii :: Double
    , ix :: Double
    , x :: Double
    , xi :: Double
    , xii :: Double
  } deriving (Show, Eq, Generic)

data Angles = Angles
  {
    ascendant :: Double
  , mc :: Double
  , armc :: Double
  , vertex :: Double
  , equatorialAscendant :: Double
  , coAscendantKoch :: Double
  , coAscendantMunkasey :: Double
  , polarAscendant :: Double
  } deriving (Show, Eq, Generic)

-- in the C lib, house systems are expected as ASCII
-- codes for specific characters (!)
-- documentation at: https://www.astro.com/swisseph/swephprg.htm#_Toc19111265
toHouseSystemFlag :: HouseSystem -> Int
toHouseSystemFlag Placidus      = ord 'P'
toHouseSystemFlag Koch          = ord 'K'
toHouseSystemFlag Porphyrius    = ord 'O'
toHouseSystemFlag Regiomontanus = ord 'R'
toHouseSystemFlag Campanus      = ord 'C'
toHouseSystemFlag Equal         = ord 'A'
toHouseSystemFlag WholeSign     = ord 'W'


-- TODO: these fromList fns could be captured in a typeclass...
fromList :: [Double] -> Coordinates
-- N.B. note that for some reason the SWE guys really like long,lat coordinates
-- though only for this one function: https://www.astro.com/swisseph/swephprg.htm#_Toc19111235
-- so we need to flip them on the way out of the C code.
fromList (sLong : sLat : c : d : e : f : _) = Coords sLat sLong c d e f
fromList _                           = error "Invalid coordinate array"

fromCuspsList :: [Double] -> HouseCusps
fromCuspsList (_ : _i : _ii : _iii : _iv : _v : _vi : _vii : _viii : _ix : _x : _xi : _xii : _)
    = Cusps _i _ii _iii _iv _v _vi _vii _viii _ix _x _xi _xii
fromCuspsList _ = error "Invalid cusps list"

fromAnglesList :: [Double] -> Angles
fromAnglesList (a : _mc : _armc : vtx : ea : cak : cam : pa : _ : _) =
    Angles a _mc _armc vtx ea cak cam pa
fromAnglesList _ = error "Invalid angles list"

planetNumber :: Planet -> PlanetNumber
planetNumber p = PlanetNumber $ CInt y
  where
    y = fromIntegral $ fromEnum p :: Int32

-- combine bitwise options. Using this to obtain the equatorial,
-- not eliptical, positions of planetary bodies:
-- 
calculationOptions :: [CalcFlag] -> CalcFlag
calculationOptions = CalcFlag . foldr ((.|.) . unCalcFlag) 0

basicCoords :: (Double, Double) -> Coordinates
basicCoords (latitude, longitude) = Coords latitude longitude 0 0 0 0

setEphemeridesPath :: S.ByteString -> IO ()
setEphemeridesPath path =
    S.useAsCString path $ \ephePath -> c_swe_set_ephe_path ephePath

julianDay :: Int -> Int -> Int -> Double -> Double
julianDay year month day hour = realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour

-- TODO: take an actual gregorian date!
calculateCoordinates :: Double -> Planet -> Either String Coordinates
calculateCoordinates time planet =
    unsafePerformIO $ allocaArray 6 $ \coords -> alloca $ \errorP -> do
        let iflgret = c_swe_calc (realToFrac time)
                                 (planetNumber planet)
                                 --(calculationOptions [swissEph, speed, equatorialPositions ])
                                 speed
                                 coords
                                 errorP

        if unCalcFlag iflgret < 0
            then do
                msg <- peekCString errorP
                return $ Left msg
            else do
                result <- peekArray 6 coords
                return $ Right $ fromList $ map realToFrac result

calculateCusps :: Double -> Coordinates -> HouseSystem -> (HouseCusps, Angles)
calculateCusps time loc sys = unsafePerformIO $ allocaArray 13 $ \cusps ->
    allocaArray 10 $ \ascmc -> do
        c_swe_houses (realToFrac time)
                     (realToFrac $ lat loc)
                     (realToFrac $ long loc)
                     (fromIntegral $ toHouseSystemFlag sys)
                     cusps
                     ascmc

        cuspsL  <- peekArray 13 cusps
        anglesL <- peekArray 10 ascmc
        return
            ( (fromCuspsList $ map realToFrac $ cuspsL)
            , (fromAnglesList $ map realToFrac $ anglesL)
            )


-- Next steps:
-- set topological center for calculations?
-- better flags: file:///Users/luis/Downloads/swe_unix_src_2.08/doc/swephprg.htm#_Toc11319063
-- notice that to calculate topocentric positions, one needs to set the TRUEPOS flag + setting the location
-- see the heading "2.3.5 Specialties > b. topocentric positions"
