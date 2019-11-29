{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeOperators #-}


module API where

import Import

import Data.Aeson
import GHC.Generics
import Servant.API
import Data.Time
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Servant as Servant
import qualified SWE as SWE

-- orphaned instances to make the SWE types JSON-serializable,
-- if we want to extract SWE, we'll have to wrap them in newtypes
-- and manually define the ToJSON instances here.
instance ToJSON SWE.Planet
instance ToJSON SWE.Coordinates
instance ToJSON SWE.HouseCusps
instance ToJSON SWE.Angles

data PlanetCoordinates = PlanetCoordinates
  {
    planet :: SWE.Planet
  , coords :: Maybe SWE.Coordinates
  } deriving (Show, Generic)

instance ToJSON PlanetCoordinates

data Astro = Astro
  {
    planets :: [PlanetCoordinates]
  , cusps   :: SWE.HouseCusps
  , angles  :: SWE.Angles
  } deriving (Show, Generic)

instance ToJSON Astro

type LatLng = (Double, Double)

data AstroRequest = AstroRequest
  {
    dob :: UTCTime
  , loc :: LatLng
  } deriving (Show, Eq, Generic)

instance FromJSON AstroRequest


-- some guidance from: https://two-wrongs.com/haskell-time-library-tutorial
sweDateTime :: UTCTime -> Double
sweDateTime (UTCTime day time) = SWE.julianDay (fromIntegral $ y) m d h
  where (y, m, d) = toGregorian day
        h         = 2.77778e-16 * (fromIntegral $ diffTimeToPicoseconds time)

planetCoordinates :: UTCTime -> SWE.Planet -> PlanetCoordinates
planetCoordinates time p =
  let parsedTime = sweDateTime time
      calc       = SWE.calculateCoordinates parsedTime p
    in case calc of
      (Right c) -> PlanetCoordinates p $ Just c
      (Left  _) -> PlanetCoordinates p $ Nothing

allPlanetCoordinates :: UTCTime -> [PlanetCoordinates]
allPlanetCoordinates t = map (planetCoordinates t) [SWE.Sun .. SWE.Chiron]

astroData :: UTCTime -> LatLng -> Astro
astroData ut ltLng =
  Astro (allPlanetCoordinates ut) c a
  where (c, a) = SWE.calculateCusps t (SWE.basicCoords ltLng) SWE.Placidus
        t      = sweDateTime ut

-- API Proper

type Api =
  "api" :>
  ( "horoscope" :> ReqBody '[JSON] AstroRequest :> Post '[JSON] Astro )

horoscope :: AstroRequest -> Servant.Handler Astro
horoscope (AstroRequest dateOfBirth location) =
  return $ astroData dateOfBirth location

apiServer :: Servant.Server Api
apiServer = horoscope

api :: Servant.Proxy Api
api = Servant.Proxy

app :: Application
app = Servant.serve api apiServer