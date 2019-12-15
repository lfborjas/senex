module Geo exposing (..)

import Json.Decode as Decode 
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Url.Builder as Builder exposing (string)
import Time as Time

type ApiKey = ApiKey String

type SessionToken = SessionToken String

type PlaceID = PlaceID String

-- from: https://korban.net/posts/elm/2018-03-16-how-to-extract-values-from-union-types-in-elm/
unApiKey (ApiKey s) = s
unSessionToken (SessionToken s) = s
unPlaceID (PlaceID s) = s


googleApiKey : ApiKey
googleApiKey = ApiKey "AIzaSyACfUA2VBhSW53_kaJT3n_eufMOSjywTFk"

{- | Place Autocomplete API integration
https://developers.google.com/places/web-service/autocomplete
-}

type alias PlaceAutocompleteRequest =
    { apiKey : ApiKey
    , sessionToken : SessionToken
    , types : String
    , query : String
    }

type GoogleAPIResponseStatus
    = OK
    | ZeroResults
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError
    | NotFound

type alias PlaceAutocompletePrediction =
    { description : String
    , placeID : PlaceID
    }

type alias PlaceAutocompleteResponse =
    { status : GoogleAPIResponseStatus
    , predictions : List PlaceAutocompletePrediction
    }

-- TODO: actually generate these!
newSessionToken : SessionToken
newSessionToken = SessionToken "5b58931d-bb69-406d-81a9-7746c300838c"

initPlaceRequest : SessionToken -> String -> PlaceAutocompleteRequest
initPlaceRequest t q = 
    { apiKey = googleApiKey
    , sessionToken = t
    , types = "(regions)"
    , query = q
    }

updatePlaceRequest : PlaceAutocompleteRequest -> String -> PlaceAutocompleteRequest
updatePlaceRequest req q =
    { req | query = q }

{- | Autocomplete Decoders -}

autocompleteResponseDecoder : Decode.Decoder PlaceAutocompleteResponse
autocompleteResponseDecoder =
    Decode.map2 PlaceAutocompleteResponse
        (Decode.field "status" googleStatusDecoder)
        (Decode.field "predictions" <| Decode.list predictionsDecoder)

googleStatusDecoder : Decode.Decoder GoogleAPIResponseStatus
googleStatusDecoder =
    let
        statusHelper : String -> Decode.Decoder GoogleAPIResponseStatus
        statusHelper v =
            case v of
                "OK" -> Decode.succeed OK
                "ZERO_RESULTS" -> Decode.succeed ZeroResults
                "OVER_QUERY_LIMIT" -> Decode.succeed OverQueryLimit
                "REQUEST_DENIED" -> Decode.succeed RequestDenied
                "INVALID_REQUEST" -> Decode.succeed InvalidRequest
                "NOT_FOUND" -> Decode.succeed NotFound
                "UNKNOWN_ERROR" ->  Decode.succeed UnknownError
                _  -> Decode.fail <| "Invalid status " ++ v
    in
    Decode.string |> Decode.andThen statusHelper

predictionsDecoder : Decode.Decoder PlaceAutocompletePrediction
predictionsDecoder = 
    Decode.succeed PlaceAutocompletePrediction
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "placeID" placeIdDecoder

placeIdDecoder : Decode.Decoder PlaceID
placeIdDecoder = 
    Decode.string |> Decode.andThen (Decode.succeed << PlaceID)

{- | Place Details API integration
https://developers.google.com/places/web-service/details#PlaceDetailsRequests
-}

type alias PlaceDetailsRequest =
    { apiKey : ApiKey
    , placeID : PlaceID
    , sessionToken : SessionToken
    , fields : String
    }

initDetailsRequest : SessionToken -> PlaceID -> PlaceDetailsRequest
initDetailsRequest existingSession placeID =
    { apiKey = googleApiKey
    , sessionToken = existingSession
    , placeID = placeID
    , fields = "geometry,name"
    }

type alias PlaceDetailsResponse =
    { status : GoogleAPIResponseStatus
    , result : PlaceDetailsResult
    }

type alias PlaceDetailsResult =
    { name : String
    , geometry : PlaceGeometry
    }

type alias PlaceGeometry =
    { location : PlaceCoordinates
    }

type alias PlaceCoordinates =
    { lat : Float
    , lng : Float
    }

placeDetailsDecoder : Decode.Decoder PlaceDetailsResponse
placeDetailsDecoder =
    Decode.map2 PlaceDetailsResponse
        (Decode.field "status" googleStatusDecoder)
        (Decode.field "result" placeResultDecoder)

placeResultDecoder : Decode.Decoder PlaceDetailsResult
placeResultDecoder =
    Decode.map2 PlaceDetailsResult
        (Decode.field "name" Decode.string)
        (Decode.field "geometry" placeGeometryDecoder)

placeGeometryDecoder : Decode.Decoder PlaceGeometry
placeGeometryDecoder =
    Decode.map PlaceGeometry
        (Decode.field "location" placeCoordinatesDecoder)

placeCoordinatesDecoder : Decode.Decoder PlaceCoordinates
placeCoordinatesDecoder =
    Decode.map2 PlaceCoordinates
        (Decode.field "lat" Decode.float)
        (Decode.field "lng" Decode.float)

{- | TimeZone API integration
    https://developers.google.com/maps/documentation/timezone
-}

type alias TimeZoneRequest =
    { apiKey : ApiKey
    , location : PlaceCoordinates
    , timestamp : Int -- seconds, not millis.
    }

type alias TimeZoneResponse =
    { status : GoogleAPIResponseStatus
    , dstOffset : Int
    , rawOffset : Int
    , timeZoneID : String
    , timeZoneName : String 
    }

unixTime : Time.Posix -> Int
unixTime t = (Time.posixToMillis t) // 1000

initTimeZoneRequest : PlaceDetailsResult -> Time.Posix -> TimeZoneRequest
initTimeZoneRequest place time =
    { apiKey = googleApiKey
    , location = place.geometry.location
    , timestamp = unixTime time
    }

zonedTime : TimeZoneResponse -> Time.Posix -> Time.Posix
zonedTime r t =
    let
        corrected = (unixTime t) + r.dstOffset + r.rawOffset
    in
    Time.millisToPosix (corrected * 1000)
    

timeZoneDecoder : Decode.Decoder TimeZoneResponse
timeZoneDecoder =
    Decode.succeed TimeZoneResponse
        |> Pipeline.required "status" googleStatusDecoder
        |> Pipeline.required "dstOffset" Decode.int
        |> Pipeline.required "rawOffest" Decode.int
        |> Pipeline.required "timeZoneId" Decode.string
        |> Pipeline.required "timeZoneName" Decode.string

{- | General Google API utils -}

type GoogleApiRequest 
    = PlaceAutocomplete PlaceAutocompleteRequest
    | PlaceDetails PlaceDetailsRequest
    | TimeZone TimeZoneRequest

-- see: https://package.elm-lang.org/packages/elm/url/latest/Url-Builder#crossOrigin

buildGoogleApiUrl : GoogleApiRequest -> String
buildGoogleApiUrl request =
    let
        path = 
            case request of
                PlaceAutocomplete _ ->
                    ["place", "autocomplete", "json"]
                PlaceDetails _ ->
                    ["place", "details", "json"]
                TimeZone _ ->
                    ["timezone", "json"]
    in
    Builder.crossOrigin 
        "https://maps.googleapis.com/maps/api"
        path
        (requestParams request)

requestParams : GoogleApiRequest -> List (Builder.QueryParameter)
requestParams r =
    case r of
        PlaceAutocomplete au -> 
            [ string "key" <| unApiKey au.apiKey
            , string "sessiontoken" <| unSessionToken au.sessionToken
            , string "types" au.types
            , string "input" au.query
            ]
    
        PlaceDetails dt ->
            [ string "key" <| unApiKey dt.apiKey
            , string "sessiontoken" <| unSessionToken dt.sessionToken
            , string "fields" dt.fields
            , string "place_id" <| unPlaceID dt.placeID
            ]
        
        TimeZone tz ->
            [ string "key" <| unApiKey tz.apiKey
            , string "location" <| (String.fromFloat tz.location.lat) ++ "," ++ (String.fromFloat tz.location.lng)
            , string "time" <| String.fromInt tz.timestamp
            ]