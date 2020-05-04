{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Geo where

import           Control.Lens          ((&), (.~), (^.))
import           Data.Aeson
import           Data.Aeson.Casing     (snakeCase)
import           Data.Aeson.TH
import           Data.Scientific
import           Data.Text             (Text, pack, unpack)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Import                hiding ((&), (.~), (^.))
import qualified Network.Wreq          as W

googleApiBase :: Text
googleApiBase = "https://maps.googleapis.com/maps/api"

newtype SessionToken = SessionToken { unSessionToken :: Text } deriving (Show)

data GoogleApiStatus = OK
                    | ZeroResults
                    | OverQueryLimit
                    | RequestDenied
                    | InvalidRequest
                    | UnknownError
                    | NotFound
                    deriving (Show, Generic)

statusFromString :: Text -> GoogleApiStatus
statusFromString v = case v of
    "OK"               -> OK
    "ZERO_RESULTS"     -> ZeroResults
    "OVER_QUERY_LIMIT" -> OverQueryLimit
    "REQUEST_DENIED"   -> RequestDenied
    "INVALID_REQUEST"  -> InvalidRequest
    "NOT_FOUND"        -> NotFound
    "UNKNOWN_ERROR"    -> UnknownError
    _                  -> UnknownError

instance FromJSON GoogleApiStatus
instance ToJSON GoogleApiStatus

data AutocompletePrediction = AutocompletePrediction
    { _description :: Text
    , _placeId     :: Text
    } deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = snakeCase . drop 1} ''AutocompletePrediction

data Autocomplete = Autocomplete
    { _status      :: GoogleApiStatus
    , _predictions :: [AutocompletePrediction]
    } deriving (Show, Generic)


instance FromJSON Autocomplete where
    parseJSON = withObject "Autocomplete" $ \o -> do
        statusString <- o .: "status"
        ps           <- o .: "predictions"

        let s = statusFromString statusString
        return $ Autocomplete s ps

instance ToJSON Autocomplete where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

{- | Types for Place Details -}

data PlaceCoordinates = PlaceCoordinates
    { _lat :: Scientific
    , _lng :: Scientific
    } deriving (Show, Generic)

deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''PlaceCoordinates

data PlaceGeometry = PlaceGeometry
    { _location :: PlaceCoordinates
    } deriving (Show, Generic)

deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''PlaceGeometry

data PlaceDetailsResult = PlaceDetailsResult
    { _name             :: Text
    , _geometry         :: PlaceGeometry
    , _formattedAddress :: Text
    } deriving (Show, Generic)

deriveJSON defaultOptions{ fieldLabelModifier = snakeCase . drop 1} ''PlaceDetailsResult

data PlaceDetails = PlaceDetails
    { _status :: GoogleApiStatus
    , _result :: PlaceDetailsResult
    } deriving (Show, Generic)

instance FromJSON PlaceDetails where
    parseJSON = withObject "PlaceDetails" $ \o -> do
        statusString <- o .: "status"
        r            <- o .: "result"

        let s = statusFromString statusString
        return $ PlaceDetails s r

instance ToJSON PlaceDetails where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


{- TimeZone types -}

data TimeZoneInfo = TimeZoneInfo
    { _status       :: GoogleApiStatus
    , _dstOffset    :: Int
    , _rawOffset    :: Int
    , _timeZoneId   :: Text
    , _timeZoneName :: Text
    } deriving (Show, Generic)

instance FromJSON TimeZoneInfo where
    parseJSON = withObject "TimeZoneInfo" $ \o -> do
        statusString <- o .: "status"
        os           <- o .: "dstOffset"
        r            <- o .: "rawOffset"
        tid          <- o .: "timeZoneId"
        tn           <- o .: "timeZoneName"

        let s = statusFromString statusString
        return $ TimeZoneInfo s os r tid tn

instance ToJSON TimeZoneInfo where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

{- Google Places Autocomplete
https://developers.google.com/places/web-service/autocomplete


Example request:
> resp <- placeAutoCompleteRequest (SessionToken "12345") ("tegucigalpa"::Text)
> encode resp
"{\"status\":\"OK\",\"predictions\":[{\"place_id\":\"ChIJUT10v7qib48R08lqIDgiz2g\",\"description\":\"Tegucigalpa, Honduras\"}]}"
-}

placeAutoCompleteRequest :: HasGoogleApiKey env => SessionToken -> Text -> RIO env Autocomplete
placeAutoCompleteRequest token query =
    let url  = googleApiBase <> "/place/autocomplete/json"
        t    = W.param "sessiontoken" .~ [unSessionToken token]
        q    = W.param "input" .~ [query]
        f    = W.param "types" .~ ["(regions)"]
        opts = W.defaults & t & q & f
    in  makeRequest opts url

{- Google Places Details
https://developers.google.com/places/web-service/autocomplete

Example request:

> resp <- placeDetailsRequest (SessionToken "123456") ("ChIJUT10v7qib48R08lqIDgiz2g"::Text)
> resp
PlaceDetails {_status = OK, _result = PlaceDetailsResult {_name = "Tegucigalpa", _geometry = PlaceGeometry {_location = PlaceCoordinates {_lat = 14.065049, _lng = -87.1715002}}, _formattedAddress = "Tegucigalpa, Honduras"}}

-}

placeDetailsRequest :: HasGoogleApiKey env => SessionToken -> Text -> RIO env PlaceDetails
placeDetailsRequest token placeID =
    let url  = googleApiBase <> "/place/details/json"
        t    = W.param "sessiontoken" .~ [unSessionToken token]
        q    = W.param "place_id" .~ [placeID]
        f    = W.param "fields" .~ ["geometry,name,formatted_address"]
        opts = W.defaults & t & q & f
    in  makeRequest opts url

{- TimeZone Api Request
https://developers.google.com/maps/documentation/timezone/intro#Requests

Example interaction

> t <- getCurrentTime
2019-12-15 19:53:03.955109 UTC
> resp <- timeZoneRequest t (PlaceCoordinates 40.7834345 (-73.9662495))
> zonedTime resp t
2019-12-15 14:53:04 UTC

Further reference:
http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock-POSIX.html
https://two-wrongs.com/haskell-time-library-tutorial
-}

-- | From: https://stackoverflow.com/questions/4194340/is-there-a-better-way-to-convert-from-utctime-to-epochtime
toUnixTime :: UTCTime -> Int
toUnixTime = round . utcTimeToPOSIXSeconds

zonedTime :: TimeZoneInfo -> UTCTime -> UTCTime
zonedTime r t = posixSecondsToUTCTime corrected
  where
    corrected = realToFrac $ (toUnixTime t) - ((_dstOffset r) + (_rawOffset r))

timeZoneRequest :: HasGoogleApiKey env => UTCTime -> PlaceCoordinates -> RIO env TimeZoneInfo
timeZoneRequest time place =
    let url = googleApiBase <> "/timezone/json"
        q =
                W.param "location"
                    .~ [pack $ show (_lat place) <> "," <> show (_lng place)]
        p    = W.param "timestamp" .~ [pack $ show (toUnixTime time)]
        opts = W.defaults & q & p
    in  makeRequest opts url

-- inspired by:
-- https://github.com/gvolpe/exchange-rates/blob/26edcc057ab2658e28aafccfc2d65a8f0d0c42a5/src/Http/Client/Forex.hs

wreqRequest :: forall a. FromJSON a => W.Options -> Text -> IO a
wreqRequest optsWithKey url = do
  response <- (W.asJSON =<< W.getWith optsWithKey (unpack url) :: IO (W.Response a))
  pure $ response ^. W.responseBody

makeRequest :: (HasGoogleApiKey env, FromJSON a) => W.Options -> Text -> RIO env a
makeRequest opts url = do
  GoogleApiKey apiKey <- view googleApiKeyL
  let optsWithKey =
        opts & W.param "key" .~ [pack apiKey]
    in liftIO $ wreqRequest optsWithKey url

