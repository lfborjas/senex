import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, succeed, andThen, map, map2, field, list, float, string, keyValuePairs)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import List as List
import Maybe as Maybe exposing (..)
import String as String
import Result as Result

{- 
  Simple app to call our Haskell backend (see ../src/Api.hs for the API definition)

  References:
  * https://guide.elm-lang.org/effects/json.html
  * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#andThen
  * https://github.com/elm/json/tree/1.1.3/src
  * https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/
  * https://package.elm-lang.org/packages/elm/core/latest/Debug#toString

-}

main =
  Browser.element
    {
      init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model = BuildingRequest HoroscopeRequest
           | Loading
           | Failure
           | Success HoroscopeResponse

defaultData : HoroscopeRequest
defaultData = 
  { dob = Just "1989-01-06T00:00:00.000Z"
  , loc = Just "14.0839053, -87.2750137"
  }

init : () -> (Model, Cmd Msg)
init _ = (BuildingRequest defaultData, Cmd.none)

type Msg = AskData 
         | NewEntry
         | GotDob String
         | GotLoc String
         | GotData (Result Http.Error HoroscopeResponse)

parseLoc : String -> List Float
parseLoc v =
  String.split "," v
    |> List.map String.toFloat
    |> List.map (Maybe.withDefault 0)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDob dob ->
      case model of
          BuildingRequest data ->
            (BuildingRequest {data | dob = Just dob}, Cmd.none)
          _ ->
            (BuildingRequest {dob = Just dob, loc = Nothing}, Cmd.none)
 
    GotLoc loc ->
      case model of
          BuildingRequest data ->
              (BuildingRequest {data | loc = Just loc}, Cmd.none)
      
          _ ->
            (BuildingRequest {dob = Nothing, loc = Just loc}, Cmd.none)

    NewEntry ->
      (BuildingRequest defaultData, Cmd.none)
 
    AskData -> 
      (Loading, getHoroscopeData model)
      -- TODO: validate before submitting!

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
          , button [ onClick NewEntry ] [ text "Try Again"]
          ]
        
      Loading ->
        text "Loading..."

      BuildingRequest r ->
        div []
          [ input [type_ "text", placeholder "Date of Birth", onInput GotDob, value (Maybe.withDefault "" r.dob) ] []
          , input [type_ "text", placeholder "Location (lat, long)", onInput GotLoc, value (Maybe.withDefault "" r.loc)] []
          , button [ onClick AskData ] [ text "Enter data!" ]
          ]       

      Success data ->
        div []
          [ button [ onClick NewEntry ] [text "New Data"]
          , astroDataTables data
          ]

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
    dob : Maybe String
  , loc : Maybe String
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
  let
    getDob d = Maybe.withDefault "" d.dob
    getLoc d = parseLoc <| Maybe.withDefault "" d.loc
  in
  Encode.object
    [
      ("dob", Encode.string (getDob data))
    , ("loc", Encode.list Encode.float (getLoc data))
    ]

getHoroscopeData : Model -> Cmd Msg
getHoroscopeData model =
  case model of
    BuildingRequest r ->
      Http.post
      { url = "http://localhost:3030/api/horoscope"
      , expect = Http.expectJson GotData horoscopeDecoder
      , body = encodeHoroscopeRequest r |> Http.jsonBody
      }

    _ ->
      Cmd.none
        
  

horoscopeDecoder : Decoder HoroscopeResponse
horoscopeDecoder = 
  Decode.map2 HoroscopeResponse
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
  keyValuePairs float |> Decode.map (List.map houseHelp)
  

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
  string |> Decode.andThen planetHelp