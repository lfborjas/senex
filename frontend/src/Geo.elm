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


{- | Place Autocomplete API integration (using our backend proxy)
https://developers.google.com/places/web-service/autocomplete
-}

type alias PlaceAutocompleteRequest =
    { sessionToken : SessionToken
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

initPlaceRequest : SessionToken -> String -> PlaceAutocompleteRequest
initPlaceRequest t q = 
    { sessionToken = t
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
        |> Pipeline.required "place_id" placeIdDecoder

placeIdDecoder : Decode.Decoder PlaceID
placeIdDecoder = 
    Decode.string |> Decode.andThen (Decode.succeed << PlaceID)

{- | Place Details API integration
https://developers.google.com/places/web-service/details#PlaceDetailsRequests
-}

type alias PlaceDetailsRequest =
    { placeID : PlaceID
    , sessionToken : SessionToken
    }

initDetailsRequest : SessionToken -> PlaceID -> PlaceDetailsRequest
initDetailsRequest existingSession placeID =
    { sessionToken = existingSession
    , placeID = placeID
    }

updateDetailsRequest : PlaceDetailsRequest -> PlaceID -> PlaceDetailsRequest
updateDetailsRequest r p =
    { r | placeID = p}

type alias PlaceDetailsResponse =
    { status : GoogleAPIResponseStatus
    , result : PlaceDetailsResult
    }

type alias PlaceDetailsResult =
    { name : String
    , formattedAddress : String
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
    Decode.map3 PlaceDetailsResult
        (Decode.field "name" Decode.string)
        (Decode.field "formatted_address" Decode.string)
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

{- | General Google API utils -}

type GoogleApiRequest 
    = PlaceAutocomplete PlaceAutocompleteRequest
    | PlaceDetails PlaceDetailsRequest

-- see: https://package.elm-lang.org/packages/elm/url/latest/Url-Builder#crossOrigin

{-
    Example URLS
    "http://localhost:3030/api/proxy/autocomplete?input=tegucigalpa&token=12345"
     "http://localhost:3030/api/proxy/placeDetails?place_id=ChIJUT10v7qib48R08lqIDgiz2g&token=12345"
-}
buildGoogleApiUrl : GoogleApiRequest -> String
buildGoogleApiUrl request =
    let
        path = 
            case request of
                PlaceAutocomplete _ ->
                    ["autocomplete"]
                PlaceDetails _ ->
                    ["placeDetails"]
    in
    Builder.crossOrigin 
        "http://localhost:3030/api/proxy"
        path
        (requestParams request)

requestParams : GoogleApiRequest -> List (Builder.QueryParameter)
requestParams r =
    case r of
        PlaceAutocomplete au -> 
            [ string "token" <| unSessionToken au.sessionToken
            , string "input" au.query
            ]
    
        PlaceDetails dt ->
            [ string "token" <| unSessionToken dt.sessionToken
            , string "place_id" <| unPlaceID dt.placeID
            ]