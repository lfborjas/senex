import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional, hardcoded)


main =
  Browser.element
    {
      init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model = Failure
           | Loading
           | NotStarted
           | Success HoroscopeResponse

init : () -> (Model, Cmd Msg)
init _ = (NotStarted, Cmd.none)

type Msg = AskData 
         | GotData (Result Http.Error HoroscopeResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AskData -> 
      (Loading, getHoroscopeData)

    GotData result ->
      case result of
        Ok r ->
          (Success r, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
      Failure ->
        div []
          [ text "Unable to load data"
          , button [ onClick AskData ] [ text "Try Again"]
          ]
        
      Loading ->
        text "Loading..."
      NotStarted ->
        div []
          [ 
            button [ onClick AskData ] [ text "Get Data" ]
          ]
      Success _ ->
        pre [] [ text "We got some data!" ]

-- Server Interactions

type alias HoroscopeRequest = 
  {
    dob : String
  --, loc : (Float, Float)
  , loc : List Float
  }

m : HoroscopeRequest
m =
  {
    dob = "1989-01-06T00:00:00.000Z"
  , loc = [14.0839053, -87.2750137]
  }

type       House = I
                 | II
                 | III
                 | IV
                 | V
                 | VI
                 | VII
                 | VIII
                 | IX
                 | X
                 | XI
                 | XII

type       Planet = Sun
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

type alias HouseCusp =
  {
    house : House
  , cusp  : Float
  }

type alias Coordinates =
  {
    lat : Float
  , long : Float
  }

type alias PlanetPosition =
  {
    planet : Planet
  , position : Coordinates
  }

-- note that in the Placidus system, the Ascendant, MC, Descendant and IC
-- correspond to the I, X, VII, and IV houses, respectively, so we ignore
-- the `angles` data provided by the server.
type alias HoroscopeResponse = 
  {
    houseCusps : List HouseCusp
  , planetaryPositions : List PlanetPosition
  }

encodeHoroscopeRequest : HoroscopeRequest -> Encode.Value
encodeHoroscopeRequest data =
  Encode.object
    [
      ("dob", Encode.string data.dob)
    , ("loc", Encode.list Encode.float data.loc)
    ]

getHoroscopeData : Cmd Msg
getHoroscopeData =
  Http.post
    { url = "http://localhost:3030/api/horoscope"
    , expect = Http.expectJson GotData horoscopeDecoder
    , body = encodeHoroscopeRequest m |> Http.jsonBody
    }

horoscopeDecoder : Decode.Decoder HoroscopeResponse
horoscopeDecoder = 
  Decode.succeed HoroscopeResponse
    |> required "cusps" (Decode.keyValuePairs Decode.float)
    |> required "planets" (Decode.keyValuePairs Decode.float)

{- cuspsDecoder : Decode.Decoder (List HouseCusp)
cuspsDecoder =
  Decode. -}
