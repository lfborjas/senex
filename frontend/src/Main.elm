import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, succeed, andThen, map, map2, field, list, float, string, keyValuePairs)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import List as List

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
            button [ onClick AskData ] [ text "Get Data for 1989/1/6" ]
          ]
      Success data ->
        astroDataTables data

astroDataTables : HoroscopeResponse -> Html Msg
astroDataTables {houseCusps, planetaryPositions} =
  div []
    [
      housesTable  houseCusps
    , planetsTable planetaryPositions
    ]

planetsTable : List PlanetPosition -> Html Msg
planetsTable positions =
  table []
    [thead []
        [tr [] 
          [ th [] [text "Planet"]
          , th [] [text "Position (lat, long)"]
          ]
        ]
    , tbody []
        (List.map planetRow positions)
    ]

planetRow : PlanetPosition -> Html Msg
planetRow {planet, position} =
  tr []
    [ td [] [text <| Debug.toString planet]
    , td [] [text <| "(" ++ Debug.toString position.lat ++ ", " ++ Debug.toString position.long ++ ")"]
    ]

houseRow : HouseCusp -> Html Msg
houseRow {house, cusp} =
  tr []
    [ td [] [text <| Debug.toString house]
    , td [] [text <| Debug.toString cusp]
    ]

housesTable : List HouseCusp -> Html Msg
housesTable cusps =
  table []
    [thead []
        [tr [] 
          [ th [] [text "House"]
          , th [] [text "Position"]
          ]
        ]
    , tbody []
        (List.map houseRow cusps)
    ]

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
                 | UnknownCusp

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
                  | UnknownPlanet

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

horoscopeDecoder : Decoder HoroscopeResponse
horoscopeDecoder = 
  map2 HoroscopeResponse
    (field "cusps" <| houseCuspsDecoder)
    (field "planets" <| list planetPositionDecoder)

houseCuspsDecoder : Decoder (List HouseCusp)
houseCuspsDecoder =
  let
      houseHelp : (String, Float) -> HouseCusp
      houseHelp (ord, pos) =
        case ord of
            "i"    -> HouseCusp I pos
            "ii"   -> HouseCusp II pos
            "iii"  -> HouseCusp III pos
            "iv"   -> HouseCusp IV pos
            "v"    -> HouseCusp V pos
            "vi"   -> HouseCusp VI pos
            "vii"  -> HouseCusp VII pos
            "viii" -> HouseCusp VIII pos
            "ix"   -> HouseCusp IX pos
            "x"    -> HouseCusp X pos
            "xi"   -> HouseCusp XI pos
            "xii"  -> HouseCusp XII pos
            _      -> HouseCusp UnknownCusp pos
  in
  keyValuePairs float |> map (List.map houseHelp)
  

planetPositionDecoder : Decoder PlanetPosition
planetPositionDecoder =
  succeed PlanetPosition
    |> required "planet" planetDecoder
    |> required "coords" coordinateDecoder

coordinateDecoder : Decoder Coordinates
coordinateDecoder =
  succeed Coordinates
    |> required "lat" float
    |> required "long" float

planetDecoder : Decoder Planet
planetDecoder =
  let 
    planetHelp : String -> Decoder Planet
    planetHelp p = 
      case p of
        "Sun"      -> succeed Sun
        "Moon"     -> succeed Moon
        "Mercury"  -> succeed Mercury
        "Venus"    -> succeed Venus
        "Mars"     -> succeed Mars
        "Jupiter"  -> succeed Jupiter
        "Saturn"   -> succeed Saturn
        "Uranus"   -> succeed Uranus
        "Neptune"  -> succeed Neptune
        "Pluto"    -> succeed Pluto
        "MeanNode" -> succeed MeanNode
        "TrueNode" -> succeed TrueNode
        "MeanApog" -> succeed MeanApog
        "OscuApog" -> succeed OscuApog
        "Earth"    -> succeed Earth
        "Chiron"   -> succeed Chiron
        _          -> succeed UnknownPlanet
  in
  string |> andThen planetHelp