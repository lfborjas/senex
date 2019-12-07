module Main exposing (Coordinates, HoroscopeRequest, HoroscopeResponse, House(..), HouseCusp, Model, Msg(..), Planet(..), PlanetPosition, astroDataTables, coordinateDecoder, defaultData, encodeHoroscopeRequest, getHoroscopeData, horoscopeDecoder, houseCuspsDecoder, houseRow, housesTable, init, main, planetDecoder, planetPositionDecoder, planetRow, planetsTable, requestHeading, subscriptions, update, view)

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

type RemoteFetch req resp
  = Loading
  | Failure req
  | Success req resp

type alias Model =
  {
    horoscopeRequest  : Maybe HoroscopeRequest
  , horoscopeResponse : Maybe (RemoteFetch HoroscopeRequest HoroscopeResponse)
  }


defaultData : HoroscopeRequest
defaultData =
    { dob = Just "1989-01-06T06:30:00.000Z"
    , loc = Just "14.0839053,-87.2750137"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( {horoscopeRequest = Just defaultData, horoscopeResponse = Nothing }, Cmd.none )


type Msg
    = GetHoroscope
    | NewHoroscope
    | GotDob String
    | GotLoc String
    | GotHoroscope HoroscopeRequest (Result Http.Error HoroscopeResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDob dob_ ->
          case model.horoscopeRequest of
            Nothing -> ({model | horoscopeRequest = Just { dob = Just dob_, loc = Nothing }}, Cmd.none)
            Just r  -> ({model | horoscopeRequest = Just { r | dob = Just dob_ }}, Cmd.none)

        GotLoc loc_ ->
          case model.horoscopeRequest of
            Nothing -> ({model | horoscopeRequest = Just { dob = Nothing, loc = Just loc_ }}, Cmd.none)
            Just r  -> ({model | horoscopeRequest = Just { r | loc = Just loc_ }}, Cmd.none)
              
        NewHoroscope ->
          ( {model | horoscopeRequest = Just defaultData}, Cmd.none )

        GetHoroscope ->
          ( {model | horoscopeResponse = Just Loading}, getHoroscopeData model )

        -- TODO: validate before submitting!
        GotHoroscope req result ->
          case result of
            Ok r ->
              ({model | horoscopeResponse = Just (Success req r)}, Cmd.none)      
            Err _ ->
              ({model | horoscopeResponse = Just (Failure req)}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
      [ viewRequestForm model 
      , viewChart model
      ]
           

viewRequestForm : Model -> Html Msg
viewRequestForm {horoscopeRequest, horoscopeResponse} =
  case horoscopeRequest of
      Nothing -> div [] []
      Just r ->
        div []
          [ input [ Attrs.type_ "text", placeholder "Date of Birth", onInput GotDob, value (Maybe.withDefault "" r.dob) ] []
          , input [ Attrs.type_ "text", placeholder "Location (lat, long)", onInput GotLoc, value (Maybe.withDefault "" r.loc) ] []
          , button [ Evts.onClick GetHoroscope ] [ Html.text "Show Chart" ]
          ]

viewChart : Model -> Html Msg
viewChart {horoscopeRequest, horoscopeResponse} =
  case horoscopeResponse of
      Nothing -> div [] [Html.text "Enter your info to see your chart!" ]
      Just fetchData ->
        case fetchData of
            Loading ->
              div [] [Html.text "Loading..."]
            Failure r ->
              div []
                [ Html.text "Unable to load data"
                , button [ Evts.onClick GetHoroscope ] [ Html.text "Try Again" ]
                ]
        
            Success entered data ->
              div []
                [ button [ Evts.onClick NewHoroscope ] [ Html.text "New Horoscope" ]
                , requestHeading entered
                , chart data
                , astroDataTables data
                ]

requestHeading : HoroscopeRequest -> Html Msg
requestHeading { dob, loc } =
    h2 []
        [ Html.text
            ("Ephemerides for "
                ++ Maybe.withDefault "" dob
                ++ " at "
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
getHoroscopeData {horoscopeRequest, horoscopeResponse} =
    case horoscopeRequest of
        Just r ->
            Http.post
                { url = "http://localhost:3030/api/horoscope"
                , expect = Http.expectJson (GotHoroscope r) horoscopeDecoder
                , body = encodeHoroscopeRequest r |> Http.jsonBody
                }

        Nothing ->
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

-- see: https://en.wikipedia.org/wiki/Astrological_aspect
type AspectName
  = Conjunction
  | Sextile
  | Square
  | Trine
  | Opposition
  | Quincunx
  | SemiSextile
  | Quintile
  | BiQuintile
  | Septile
  | SemiSquare
  | Novile
  | Sesquisquare -- Trioctile

type alias Aspect = { name : AspectName, maxOrb : Float, angle : Float}

type alias HoroscopeAspect =
  {
    aspect : Aspect
  , planets : (PlanetPosition, PlanetPosition)
  }

-- see: https://en.wikipedia.org/wiki/Astrological_sign#Western_zodiac_signs
type alias ZodiacSign = 
  { name : ZodiacSignName
  , longitude : Float
  , element : ClassicalElement
  }

-- using the simplified orbs adopted by astro.com and Liz Greene:
-- https://www.astro.com/astrology/in_aspect_e.htm
majorAspects : List Aspect
majorAspects =
  [
    {name = Conjunction, angle = 0.0, maxOrb = 10.0}
  , {name = Sextile, angle = 60.0, maxOrb = 6.0 }
  , {name = Square, angle = 90.0, maxOrb = 10.0}
  , {name = Trine, angle = 120.0, maxOrb = 10.0}
  , {name = Opposition, angle = 180.0, maxOrb = 10.0}
  ]

minorAspects : List Aspect
minorAspects =
  [ 
    {name = SemiSquare, angle = 45.0, maxOrb = 3.0}
  , {name = Sesquisquare, angle = 135.0, maxOrb = 3.0}
  , {name = SemiSextile, angle = 30.0, maxOrb = 3.0}
  , {name = Quincunx, angle = 150.0, maxOrb = 3.0}
  , {name = Quintile, angle = 72.0, maxOrb = 2.0}
  , {name = BiQuintile, angle = 144.0, maxOrb = 2.0}
  ]

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

houseAngle : House -> List HouseCusp -> Maybe Float
houseAngle h cusps =
  let
    isAngle : House -> HouseCusp -> Maybe HouseCusp
    isAngle h_ c_ = 
      case c_.house == h_ of
        True  -> Just c_
        False -> Nothing
  in
  List.filterMap (isAngle h) cusps
    |> List.head
    |> Maybe.andThen (\c->Just c.cusp)

ascendantAngle = houseAngle I

calculateAspects : List PlanetPosition -> List Aspect -> List HoroscopeAspect
calculateAspects planetPositions aspects = []
{-   let
      allAspects : PlanetPosition -> List HoroscopeAspect
      allAspects {planet, position} =
        

  in
  
  List.concatMap allAspects -}

chart : HoroscopeResponse -> Html Msg
chart {houseCusps, planetaryPositions} =
  let
    width  = 666
    center = width / 2
    r      = width * 0.42
    o      = ascendantAngle houseCusps |> Maybe.withDefault 0.0
    container =  { centerX = center, centerY = center, radius = r, offset = (180 - o) }
    aspects_  = calculateAspects planetaryPositions <| List.append majorAspects minorAspects
  in
  svg
    [ SvgAttrs.width (String.fromFloat width), SvgAttrs.height (String.fromFloat width) ]
    [ g [SvgAttrs.id "radix"]
        [ zodiac  container
        , houses  container houseCusps
        , planets container planetaryPositions
        ] 
    ]

zodiac : Circle -> Svg Msg
zodiac containerCircle =
  g [SvgAttrs.id "zodiac"]
    [ zodiacCircle containerCircle
    , g [SvgAttrs.id "signs"] (zodiacSigns containerCircle)
    ]

houses : Circle -> List HouseCusp -> Svg Msg
houses container housesData =
  let
      containerCircle = { container | radius = container.radius - 33}
  in
  g [SvgAttrs.id "housesCircle"]
    [ housesCircle containerCircle
    , g [SvgAttrs.id "houses"] (drawHouses  containerCircle housesData)
    --, g [SvgAttrs.id "ruler"]  (drawDegrees containerCircle (List.range 0 360))
    ]

planets : Circle -> List PlanetPosition -> Svg Msg
planets containerCircle planetsData =
  g [SvgAttrs.id "planetsCircle"]
    [ g [SvgAttrs.id "planets"] (drawPlanets containerCircle planetsData)
    ]

housesCircle : Circle -> Svg Msg
housesCircle {centerX, centerY, radius} =
  circle [ cx (String.fromFloat centerX)
         , cy (String.fromFloat centerY)
         , r (String.fromFloat radius)
         , fill "none"
         , stroke "#444"
         , strokeWidth "1"
         ]
        []  

drawPlanets : Circle -> List PlanetPosition -> List (Svg Msg)
drawPlanets c d = List.map (drawPlanet c) d

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
    , drawTextAtDegree container (houseText house)  "font: italic 15px serif; fill: #333;" -(cusp+8.5)
    ]

drawPlanet container {planet, position} =
  g []
    [ drawTextAtDegree container (planetText planet) "font: 15px sans-serif; fill: #666;" -position.long ]

zodiacCircle : Circle -> Svg Msg
zodiacCircle {centerX, centerY, radius} =
  circle [cx (String.fromFloat centerX), cy (String.fromFloat centerY), r (String.fromFloat radius), fill "none", stroke "#333", strokeWidth "2"] []

zodiacSigns : Circle -> List (Svg Msg)
zodiacSigns c = List.map (zodiacSign c) westernSigns

zodiacSign : Circle -> ZodiacSign -> Svg Msg
zodiacSign container sign =
  g []
   [ 
      Svg.path [d (buildSlicePath container 30.0 0.125 (-sign.longitude)), fill (elementColor sign.element), strokeWidth "0", stroke "none"] []
    , signGlyph 
        container
        { sign | longitude = -(sign.longitude + 15.0) }
   ]

planetText : Planet -> String
planetText p =
  case p of
      Sun -> "☉"
      Moon -> "☽"
      Mercury -> "☿"
      Venus -> "♀︎"
      Earth_ -> ""
      Mars -> "♂︎"
      Jupiter -> "♃"
      Saturn -> "♄"
      Uranus -> "♅"
      Neptune -> "♆"
      Pluto -> "♇"
      _ -> ""

houseText : House -> String
houseText h =
  case h of
      I -> "ASC"
      IV -> "IC"
      VII -> "DSC"
      X -> "MC"
      _ -> Debug.toString h
          

-- Helper functions for the crazy math
type alias Circle = { centerX: Float, centerY: Float, radius : Float, offset: Float}
type alias Angle = Float
type alias Cartesian = { x: Float, y: Float}

polarToCartesian : Circle -> Angle -> Cartesian
polarToCartesian { centerX, centerY, radius, offset} angle =
  let
    -- all coordinates should be relative to the offset, which is the angle
    -- between the Ascendant and 180 degrees from the zero point of the container
    -- (which isn't really Aries, as Aries will also be off-set.)
    offsetAngle = angle - offset
    (x_, y_)    = fromPolar (radius, degrees offsetAngle)
  in
  {x = centerX + x_, y = centerY + y_}

elementColor : ClassicalElement -> String
elementColor element =
  let
    color = case element of
        Earth -> Color.darkGreen
        Air   -> Color.yellow
        Fire  -> Color.lightRed
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

signGlyph : Circle -> ZodiacSign -> Svg Msg
signGlyph  container {name, longitude, element} =
  let
    inner = { container | radius = container.radius - 18.0 }
    loc = polarToCartesian inner longitude
    size = 20.0
    svgName = 
      case name of
        Aries -> "Aries.svg#svg602"
        Taurus -> "Taurus.svg#svg614"
        Gemini -> "Gemini.svg#svg620"
        Cancer -> "Cancer.svg#svg626"
        Leo -> "Leo.svg#svg632"
        Virgo -> "Virgo.svg#svg638"
        Libra -> "Libra.svg#svg644"
        Scorpio -> "Scorpio.svg#svg650"
        Sagittarius -> "Sagittarius.svg#svg656"
        Capricorn -> "Capricorn.svg#svg1"
        Aquarius -> "Aquarius.svg#svg668"
        Pisces -> "Pisces.svg#svg674"
    svgPath = "/assets/img/" ++ svgName
  in
  Svg.use 
    [ SvgAttrs.xlinkHref svgPath
    , SvgAttrs.width <| String.fromFloat size
    , SvgAttrs.height <| String.fromFloat size
    , SvgAttrs.x <| String.fromFloat (loc.x-size/2)
    , SvgAttrs.y <| String.fromFloat (loc.y-size/2)
    ]
    []

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

All external SVG files are from the public domain, linked here:
https://en.wikipedia.org/wiki/Astrological_symbols#Miscellaneous_symbols

For time queries:
https://developers.google.com/maps/documentation/timezone/start 

for addresses/geocoding:
https://developers.google.com/places/web-service/autocomplete
https://developers.google.com/maps/documentation/geocoding/best-practices (use places and then latLong with placeID)
-}