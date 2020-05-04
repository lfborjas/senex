{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}


module API where

import           Import

import           Data.Aeson
import           Data.Scientific             (fromFloatDigits)
import           Data.Time
import           Geo
import           Network.Wai
import           Network.Wai.Middleware.Cors
import qualified Servant                     as Servant
import           Servant.API
import qualified SWE                         as SWE

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
  ( "horoscope" :> ReqBody '[JSON] AstroRequest :> Post '[JSON] Astro :<|>
    "proxy" :> "autocomplete" :> QueryParam' '[Required] "input" Text :> QueryParam' '[Required] "token" Text :> Get '[JSON] Autocomplete :<|>
    "proxy" :> "placeDetails" :> QueryParam' '[Required] "place_id" Text :> QueryParam' '[Required] "token" Text :> Get '[JSON] PlaceDetails
  )


-- RIO's default name for the "environment" is App, which is fine and all,
-- but confusing in the Servant world. Introducing an alias here.
type AppCtx = App
-- From: https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html#custom-monad
type AppM = ReaderT AppCtx Servant.Handler

horoscope :: AstroRequest -> AppM Astro
horoscope (AstroRequest dateOfBirth location@(latitude, longitude)) = do
  env <- ask
  timezoneInfo <- liftIO $ runRIO env $ timeZoneRequest dateOfBirth (PlaceCoordinates (fromFloatDigits latitude) (fromFloatDigits longitude))
  return $ astroData (zonedTime timezoneInfo dateOfBirth) location

autocompleteHandler :: Text -> Text -> AppM Autocomplete
autocompleteHandler q token = do
  env <- ask
  liftIO $ runRIO env $ placeAutoCompleteRequest (SessionToken token) q

placeDetailsHandler :: Text -> Text -> AppM PlaceDetails
placeDetailsHandler p token = do
  env <- ask
  liftIO $ runRIO env $ placeDetailsRequest (SessionToken token) p

apiServer :: Servant.ServerT Api AppM
apiServer = horoscope
  :<|> autocompleteHandler
  :<|> placeDetailsHandler

api :: Servant.Proxy Api
api = Servant.Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: AppCtx -> Application
app env = cors (const $ Just policy) $ hoistedServer
  where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
        hoistedServer = Servant.serve api $ Servant.hoistServer api (nt env) apiServer
