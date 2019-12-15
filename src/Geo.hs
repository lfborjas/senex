{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# Language RankNTypes, ScopedTypeVariables #-}

module Geo where

import           Control.Lens               (traverse, (&), (.~), (<&>), (?~),
                                            (^.), (^..))

import           Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH
import qualified Data.Aeson.Types as DT
import Data.Scientific
import Data.Text (Text, unpack)
import GHC.Generics
import qualified Network.Wreq as W
import System.IO

-- inspired by: 
-- https://github.com/gvolpe/exchange-rates/blob/26edcc057ab2658e28aafccfc2d65a8f0d0c42a5/src/Http/Client/Forex.hs

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
statusFromString v =
    case v of
        "OK" -> OK
        "ZERO_RESULTS" -> ZeroResults
        "OVER_QUERY_LIMIT" -> OverQueryLimit
        "REQUEST_DENIED" -> RequestDenied
        "INVALID_REQUEST" -> InvalidRequest
        "NOT_FOUND" -> NotFound
        "UNKNOWN_ERROR" ->  UnknownError
        _ -> UnknownError

instance FromJSON GoogleApiStatus
instance ToJSON GoogleApiStatus

data AutocompletePrediction = AutocompletePrediction
    { _description :: Text
    , _placeId :: Text
    } deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = snakeCase . drop 1} ''AutocompletePrediction

data Autocomplete = Autocomplete
    { _status :: GoogleApiStatus
    , _predictions :: [AutocompletePrediction]
    } deriving (Show, Generic)


instance FromJSON Autocomplete where
    parseJSON = withObject "Autocomplete" $ \o -> do
        statusString <- o .: "status"
        ps           <- o .: "predictions"

        let s = statusFromString statusString
        return $ Autocomplete s ps

instance ToJSON Autocomplete where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

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
    { _name :: Text
    , _geometry :: PlaceGeometry
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
    toJSON = genericToJSON defaultOptions  {fieldLabelModifier = drop 1}

{- Google Places Autocomplete
https://developers.google.com/places/web-service/autocomplete


Example request:
> resp <- placeAutoCompleteRequest (SessionToken "12345") ("tegucigalpa"::Text)
> encode resp
"{\"status\":\"OK\",\"predictions\":[{\"place_id\":\"ChIJUT10v7qib48R08lqIDgiz2g\",\"description\":\"Tegucigalpa, Honduras\"}]}"
-}

placeAutoCompleteRequest :: SessionToken -> Text -> IO Autocomplete
placeAutoCompleteRequest token query =
    let url  = googleApiBase <> "/place/autocomplete/json"
        t    = W.param "sessiontoken" .~ [unSessionToken token]
        q    = W.param "input" .~ [query]
        f    = W.param "types" .~ ["(regions)"]
        opts = W.defaults & t & q & f
    in
    makeRequest opts url

{- Google Places Details
https://developers.google.com/places/web-service/autocomplete

Example request:

> resp <- placeDetailsRequest (SessionToken "123456") ("ChIJUT10v7qib48R08lqIDgiz2g"::Text)
> resp
PlaceDetails {_status = OK, _result = PlaceDetailsResult {_name = "Tegucigalpa", _geometry = PlaceGeometry {_location = PlaceCoordinates {_lat = 14.065049, _lng = -87.1715002}}, _formattedAddress = "Tegucigalpa, Honduras"}}

-}

placeDetailsRequest :: SessionToken -> Text -> IO PlaceDetails
placeDetailsRequest token placeID =
    let url = googleApiBase <> "/place/details/json"
        t   = W.param "sessiontoken" .~ [unSessionToken token]
        q   = W.param "place_id" .~ [placeID]
        f   = W.param "fields" .~ ["geometry,name,formatted_address"]
        opts = W.defaults & t & q & f
    in
    makeRequest opts url

makeRequest :: forall a . FromJSON a => W.Options -> Text -> IO a
makeRequest opts url =
    let optsWithKey = opts
            & W.param  "key" .~ ["AIzaSyACfUA2VBhSW53_kaJT3n_eufMOSjywTFk"]
    in
    (^. W.responseBody) <$> (W.asJSON =<< W.getWith optsWithKey (unpack url) :: IO (W.Response a))