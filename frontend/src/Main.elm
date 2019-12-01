module Main exposing (Coordinates, HoroscopeRequest, HoroscopeResponse, House(..), HouseCusp, Model(..), Msg(..), Planet(..), PlanetPosition, astroDataTables, coordinateDecoder, defaultData, encodeHoroscopeRequest, getHoroscopeData, horoscopeDecoder, houseCuspsDecoder, houseRow, housesTable, init, main, planetDecoder, planetPositionDecoder, planetRow, planetsTable, requestHeading, subscriptions, update, view)

import Browser
import Html as Html exposing (..)
import Svg as Svg exposing (..)
import Html.Attributes as Attrs exposing (..)
import Svg.Attributes as SvgAttrs exposing (..)
import Html.Events as Evts exposing (..)
import Svg.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, float, keyValuePairs, list, map, map2, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List as List
import Maybe as Maybe exposing (..)
import Result as Result
import String as String
import Color exposing (Color)



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
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = BuildingRequest HoroscopeRequest
    | Loading
    | Failure HoroscopeRequest
    | Success HoroscopeRequest HoroscopeResponse


defaultData : HoroscopeRequest
defaultData =
    { dob = Just "1989-01-06T00:00:00.000Z"
    , loc = Just "14.0839053,-87.2750137"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( BuildingRequest defaultData, Cmd.none )


type Msg
    = AskData
    | NewEntry
    | GotDob String
    | GotLoc String
    | GotData HoroscopeRequest (Result Http.Error HoroscopeResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDob dob ->
            case model of
                BuildingRequest data ->
                    ( BuildingRequest { data | dob = Just dob }, Cmd.none )

                _ ->
                    ( BuildingRequest { dob = Just dob, loc = Nothing }, Cmd.none )

        GotLoc loc ->
            case model of
                BuildingRequest data ->
                    ( BuildingRequest { data | loc = Just loc }, Cmd.none )

                _ ->
                    ( BuildingRequest { dob = Nothing, loc = Just loc }, Cmd.none )

        NewEntry ->
            ( BuildingRequest defaultData, Cmd.none )

        AskData ->
            ( Loading, getHoroscopeData model )

        -- TODO: validate before submitting!
        GotData req result ->
            case result of
                Ok r ->
                    ( Success req r, Cmd.none )

                Err _ ->
                    ( Failure req, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure request ->
            div []
                [ Html.text "Unable to load data"
                , button [ Evts.onClick NewEntry ] [ Html.text "Try Again" ]
                ]

        Loading ->
            Html.text "Loading..."

        BuildingRequest r ->
            div []
                [ input [ Attrs.type_ "text", placeholder "Date of Birth", onInput GotDob, value (Maybe.withDefault "" r.dob) ] []
                , input [ Attrs.type_ "text", placeholder "Location (lat, long)", onInput GotLoc, value (Maybe.withDefault "" r.loc) ] []
                , button [ Evts.onClick AskData ] [ Html.text "Enter data!" ]
                ]

        Success req data ->
            div []
                [ button [ Evts.onClick NewEntry ] [ Html.text "New Data" ]
                , requestHeading req
                , chart data
                , astroDataTables data
                ]


requestHeading : HoroscopeRequest -> Html Msg
requestHeading { dob, loc } =
    h2 []
        [ Html.text
            ("Ephemerides for "
                ++ Maybe.withDefault "" dob
                ++ "--"
                ++ Maybe.withDefault "" loc
            )
        ]


astroDataTables : HoroscopeResponse -> Html Msg
astroDataTables { houseCusps, planetaryPositions } =
    div []
        [ housesTable houseCusps
        , planetsTable planetaryPositions
        ]


planetsTable : List PlanetPosition -> Html Msg
planetsTable positions =
    table []
        [ thead []
            [ tr []
                [ th [] [ Html.text "Planet" ]
                , th [] [ Html.text "Position (lat, long)" ]
                ]
            ]
        , tbody []
            (List.map planetRow positions)
        ]


planetRow : PlanetPosition -> Html Msg
planetRow { planet, position } =
    tr []
        [ td [] [ Html.text <| Debug.toString planet ]
        , td [] [ Html.text <| "(" ++ Debug.toString position.lat ++ ", " ++ Debug.toString position.long ++ ")" ]
        ]


houseRow : HouseCusp -> Html Msg
houseRow { house, cusp } =
    tr []
        [ td [] [ Html.text <| Debug.toString house ]
        , td [] [ Html.text <| Debug.toString cusp ]
        ]


housesTable : List HouseCusp -> Html Msg
housesTable cusps =
    table []
        [ thead []
            [ tr []
                [ th [] [ Html.text "House" ]
                , th [] [ Html.text "Position" ]
                ]
            ]
        , tbody []
            (List.map houseRow cusps)
        ]



-- Server Interactions


type alias HoroscopeRequest =
    { dob : Maybe String
    , loc : Maybe String
    }


type House
    = I
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


type Planet
    = Sun
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
    | Earth_
    | Chiron
    | UnknownPlanet


type alias HouseCusp =
    { house : House
    , cusp : Float
    }


type alias Coordinates =
    { lat : Float
    , long : Float
    }


type alias PlanetPosition =
    { planet : Planet
    , position : Coordinates
    }



-- note that in the Placidus system, the Ascendant, MC, Descendant and IC
-- correspond to the I, X, VII, and IV houses, respectively, so we ignore
-- the `angles` data provided by the server.


type alias HoroscopeResponse =
    { houseCusps : List HouseCusp
    , planetaryPositions : List PlanetPosition
    }


encodeHoroscopeRequest : HoroscopeRequest -> Encode.Value
encodeHoroscopeRequest data =
    let
        parseLoc : String -> List Float
        parseLoc v =
            String.split "," v
                |> List.map String.toFloat
                |> List.map (Maybe.withDefault 0)

        getDob d =
            Maybe.withDefault "" d.dob

        getLoc d =
            parseLoc <| Maybe.withDefault "" d.loc
    in
    Encode.object
        [ ( "dob", Encode.string (getDob data) )
        , ( "loc", Encode.list Encode.float (getLoc data) )
        ]


getHoroscopeData : Model -> Cmd Msg
getHoroscopeData model =
    case model of
        BuildingRequest r ->
            Http.post
                { url = "http://localhost:3030/api/horoscope"
                , expect = Http.expectJson (GotData r) horoscopeDecoder
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
        houseHelp : ( String, Float ) -> HouseCusp
        houseHelp ( ord, pos ) =
            case ord of
                "i" ->
                    HouseCusp I pos

                "ii" ->
                    HouseCusp II pos

                "iii" ->
                    HouseCusp III pos

                "iv" ->
                    HouseCusp IV pos

                "v" ->
                    HouseCusp V pos

                "vi" ->
                    HouseCusp VI pos

                "vii" ->
                    HouseCusp VII pos

                "viii" ->
                    HouseCusp VIII pos

                "ix" ->
                    HouseCusp IX pos

                "x" ->
                    HouseCusp X pos

                "xi" ->
                    HouseCusp XI pos

                "xii" ->
                    HouseCusp XII pos

                _ ->
                    HouseCusp UnknownCusp pos
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
                "Sun" ->
                    succeed Sun

                "Moon" ->
                    succeed Moon

                "Mercury" ->
                    succeed Mercury

                "Venus" ->
                    succeed Venus

                "Mars" ->
                    succeed Mars

                "Jupiter" ->
                    succeed Jupiter

                "Saturn" ->
                    succeed Saturn

                "Uranus" ->
                    succeed Uranus

                "Neptune" ->
                    succeed Neptune

                "Pluto" ->
                    succeed Pluto

                "MeanNode" ->
                    succeed MeanNode

                "TrueNode" ->
                    succeed TrueNode

                "MeanApog" ->
                    succeed MeanApog

                "OscuApog" ->
                    succeed OscuApog

                "Earth" ->
                    succeed Earth_

                "Chiron" ->
                    succeed Chiron

                _ ->
                    succeed UnknownPlanet
    in
    string |> Decode.andThen planetHelp


-- | Chart drawing and associated types

type ZodiacSignName
  = Aries
  | Taurus
  | Gemini
  | Cancer
  | Leo
  | Virgo
  | Libra
  | Scorpio
  | Sagittarius
  | Capricorn
  | Aquarius
  | Pisces

type ClassicalElement
  = Earth
  | Air
  | Fire
  | Water

-- see: https://en.wikipedia.org/wiki/Astrological_sign#Western_zodiac_signs
type alias ZodiacSign = 
  { name : ZodiacSignName
  , longitude : Float
  , element : ClassicalElement
  }

westernSigns : List ZodiacSign
westernSigns =
  [
    {name = Aries,       longitude = 0.0, element = Fire}
  , {name = Taurus,      longitude = 30.0, element = Earth}
  , {name = Gemini,      longitude = 60.0, element = Air}
  , {name = Cancer,      longitude = 90.0, element = Water}
  , {name = Leo,         longitude = 120.0, element = Fire}
  , {name = Virgo,       longitude = 150.0, element = Earth}
  , {name = Libra,       longitude = 180.0, element = Air}
  , {name = Scorpio,     longitude = 210.0, element = Water}
  , {name = Sagittarius, longitude = 240.0, element = Fire}
  , {name = Capricorn,   longitude = 270.0, element = Earth}
  , {name = Aquarius,    longitude = 300.0, element = Air}
  , {name = Pisces,      longitude = 330.0, element = Water}
  ]

chart : HoroscopeResponse -> Html Msg
chart {houseCusps, planetaryPositions} =
  let
    width = 666    
  in
  svg
    [ SvgAttrs.width (String.fromFloat width), SvgAttrs.height (String.fromFloat width) ]
    [ g [SvgAttrs.id "radix"] 
        [ zodiac width
        , houses width houseCusps
        ] 
    ]

zodiac : Float -> Svg Msg
zodiac containerWidth =
  let
    center = containerWidth / 2
    r      = containerWidth * 0.42
    containerCircle = { centerX = center, centerY = center, radius = r }
  in
  g [SvgAttrs.id "zodiac"]
    [ zodiacCircle containerCircle
    , g [SvgAttrs.id "signs"] (zodiacSigns containerCircle)
    ]

houses : Float -> List HouseCusp -> Svg Msg
houses containerWidth housesData =
  let
      center = containerWidth / 2
      r      = containerWidth * 0.37
      containerCircle = { centerX = center, centerY = center, radius = r}
  in
  g [SvgAttrs.id "housesCircle"]
    [ housesCircle containerCircle
    , g [SvgAttrs.id "houses"] (drawHouses  containerCircle housesData)
    , g [SvgAttrs.id "ruler"]  (drawDegrees containerCircle (List.range 0 360))
    ]

housesCircle : Circle -> Svg Msg
housesCircle {centerX, centerY, radius} =
  circle [cx (String.fromFloat centerX), cy (String.fromFloat centerY), r (String.fromFloat radius), fill "none", stroke "#444", strokeWidth "1"] []  

drawHouses : Circle -> List HouseCusp -> List (Svg Msg)
drawHouses c d = List.map (drawHouse c) d

drawDegrees : Circle -> List Int -> List (Svg Msg)
drawDegrees c d = List.map (drawDegree c) d

drawDegree : Circle -> Int -> Svg Msg
drawDegree container dg =
  g []
    [ Svg.path [d (drawLinePath container (toFloat dg) (0.128*1.5)), fill "none", strokeWidth "1", stroke (Color.toCssString Color.lightGray)] []
    , drawTextAtDegree container (String.fromInt dg)  "font: italic 2px serif; fill: #222;" -(toFloat dg)
    ]

drawHouse : Circle -> HouseCusp -> Svg Msg
drawHouse container {house, cusp} =
  g [SvgAttrs.id (Debug.toString house)] 
    [ Svg.path [d (drawLinePath container -cusp (0.128*2.0)), fill "none", strokeWidth "2", stroke (Color.toCssString Color.black) ] []
    , drawTextAtDegree container (Debug.toString house)  "font: italic 15px serif; fill: #333;" -(cusp+8.5)
    ]

zodiacCircle : Circle -> Svg Msg
zodiacCircle {centerX, centerY, radius} =
  circle [cx (String.fromFloat centerX), cy (String.fromFloat centerY), r (String.fromFloat radius), fill "none", stroke "#333", strokeWidth "2"] []

zodiacSigns : Circle -> List (Svg Msg)
zodiacSigns c = List.map (zodiacSign c) westernSigns

zodiacSign : Circle -> ZodiacSign -> Svg Msg
zodiacSign container {name, longitude, element} =
  Svg.path [d (buildSlicePath container 30.0 0.125 (-longitude)), fill (elementColor element), strokeWidth "0", stroke "none"]
   []

-- Helper functions for the crazy math
type alias Circle = { centerX: Float, centerY: Float, radius : Float}
type alias Angle = Float
type alias Cartesian = { x: Float, y: Float}

polarToCartesian : Circle -> Angle -> Cartesian
polarToCartesian { centerX, centerY, radius } angle =
  let
      (x_, y_) = fromPolar (radius, degrees angle)
  in
  {x = centerX + x_, y = centerY + y_}

elementColor : ClassicalElement -> String
elementColor element =
  let
    color = case element of
        Earth -> Color.darkGreen
        Air   -> Color.yellow
        Fire  -> Color.lightOrange
        Water -> Color.lightBlue
  in
  Color.toCssString color
        

-- from: https://stackoverflow.com/a/43211655
buildSlicePath : Circle -> Float -> Float -> Float -> String
buildSlicePath containerCircle length spreadRatio longitude =
  let
      endAngle = longitude
      startAngle = longitude - length
      spread     = containerCircle.radius * spreadRatio
      innerCircle = {containerCircle | radius = containerCircle.radius - spread}
      innerStart = polarToCartesian innerCircle endAngle
      innerEnd   = polarToCartesian innerCircle startAngle
      outerStart = polarToCartesian containerCircle endAngle
      outerEnd   = polarToCartesian containerCircle startAngle
      largeArcFlag = 
        if endAngle >= startAngle then
          if (endAngle - startAngle <= 180.0) then "0" else "1"
        else
          if (endAngle + 360.0) - startAngle <= 180.0 then "0" else "1"
      elements = 
        [ "M", String.fromFloat outerStart.x, String.fromFloat outerStart.y
        , "A", String.fromFloat containerCircle.radius, String.fromFloat containerCircle.radius, "0", largeArcFlag, "0", String.fromFloat outerEnd.x, String.fromFloat outerEnd.y
        , "L", String.fromFloat innerEnd.x, String.fromFloat innerEnd.y
        , "A", String.fromFloat innerCircle.radius, String.fromFloat innerCircle.radius, "0", largeArcFlag, "1", String.fromFloat innerStart.x, String.fromFloat innerStart.y
        , "L", String.fromFloat outerStart.x, String.fromFloat outerStart.y, "Z"
        ]
  in
  String.join " " elements
  
drawLinePath : Circle -> Float -> Float -> String
drawLinePath containerCircle longitude lineLength =
  let
      startAngle = longitude
      spread     = containerCircle.radius * lineLength
      innerCircle = {containerCircle | radius = containerCircle.radius - spread}
      innerEnd   = polarToCartesian innerCircle startAngle
      outerEnd   = polarToCartesian containerCircle startAngle
      elements = 
        [ "M", String.fromFloat outerEnd.x, String.fromFloat outerEnd.y
        , "L", String.fromFloat innerEnd.x, String.fromFloat innerEnd.y
        ]
  in
  String.join " " elements

drawTextAtDegree : Circle -> String -> String -> Float -> Svg Msg
drawTextAtDegree containerCircle text style longitude =
  let
    spread = containerCircle.radius * (0.128*1.5)
    loc = polarToCartesian {containerCircle | radius = containerCircle.radius - spread} longitude
  in
    Svg.text_ [SvgAttrs.x (String.fromFloat loc.x), SvgAttrs.y (String.fromFloat loc.y), SvgAttrs.style style]
      [ Svg.text text ]
  

{- References:
https://cdn.rawgit.com/Kibo/AstroChart/master/project/examples/radix/radix_2016_11_15.html
https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Basic_Transformations
https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Texts
https://en.wikipedia.org/wiki/Astrological_sign
https://en.wikipedia.org/wiki/Horoscope#Houses_2
In the end, we'll just need to rotate things relative to the Ascendant:
https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
https://github.com/avh4/elm-color/tree/1.0.0

Local references:
file:///Users/luis/Downloads/swe_unix_src_2.08/doc/swisseph.htm#_Toc502931312
file:///Users/luis/Downloads/swe_unix_src_2.08/doc/swephprg.htm#_Toc476664303
-}