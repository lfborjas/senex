module Main exposing (Coordinates, HoroscopeRequest, HoroscopeResponse, House(..), HouseCusp, Model, Msg(..), Planet(..), PlanetPosition, astroDataTables, coordinateDecoder, defaultData, encodeHoroscopeRequest, getHoroscopeData, horoscopeDecoder, houseCuspsDecoder, houseRow, housesTable, init, main, planetDecoder, planetPositionDecoder, planetRow, planetsTable, requestHeading, subscriptions, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Browser
import Color exposing (Color)
import Html as Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Evts exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, float, keyValuePairs, list, map, map2, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List as List
import List.Extra exposing (takeWhile, uniquePairs)
import Maybe as Maybe exposing (..)
import Result as Result
import String as String
import Svg as Svg exposing (..)
import Svg.Attributes as SvgAttrs exposing (..)
import Svg.Events exposing (..)
import Geo exposing (..)
import Random as Random
import UUID as UUID
import Time as Time
import Iso8601 as ISO

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


type alias AspectsList =
    List (Maybe HoroscopeAspect)

type alias Model =
    { horoscopeRequest : Maybe HoroscopeRequest
    , horoscopeResponse : Maybe (RemoteFetch HoroscopeRequest HoroscopeResponse)
    , horoscopeAspects : Maybe AspectsList
    , navbarState : Navbar.State
    -- Google API interactions:
    , autocompleteRequest : Maybe PlaceAutocompleteRequest
    , autocompleteResponse : Maybe (RemoteFetch PlaceAutocompleteRequest PlaceAutocompleteResponse)
    , placeDetailsRequest : Maybe PlaceDetailsRequest
    , placeDetailsResponse : Maybe (RemoteFetch PlaceDetailsRequest PlaceDetailsResponse)
    , timeZoneRequest : Maybe TimeZoneRequest
    , timeZoneResponse : Maybe (RemoteFetch TimeZoneRequest TimeZoneResponse)
    , autocompleteSessionToken : SessionToken
    , tokenSeed : Random.Seed
    , partialTimeInput : Maybe String
    }


defaultData : HoroscopeRequest
defaultData =
    { dob = Just "1989-01-06T06:30:00.000Z"
    , loc = Just "14.0839053,-87.2750137"
    }

generateSessionToken : Random.Seed -> (String, Random.Seed)
generateSessionToken s =
  let
      (uuid, ns) = Random.step UUID.generator s
  in
  (UUID.toString uuid, ns)

init : Int -> ( Model, Cmd Msg )
init randomSeed =
    let
        (token, nextSeed) = generateSessionToken <| Random.initialSeed randomSeed
        ( ns, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { horoscopeRequest = Just defaultData
      , horoscopeResponse = Nothing, horoscopeAspects = Nothing
      , navbarState = ns 
      , autocompleteRequest = Nothing
      , autocompleteResponse = Nothing
      , placeDetailsRequest = Nothing
      , placeDetailsResponse = Nothing
      , timeZoneRequest = Nothing
      , timeZoneResponse = Nothing
      , autocompleteSessionToken = SessionToken token
      , tokenSeed = nextSeed
      , partialTimeInput = Nothing
      }
    , navbarCmd
    )


type Msg
    = GetHoroscope
    | NewHoroscope
    | GotDob String
    | GotLoc String
    | GotHoroscope HoroscopeRequest (Result Http.Error HoroscopeResponse)
    | NavbarMsg Navbar.State
    | UpdatedPlaceInput String
    | UpdatedTimeInput String
    | PlaceSelected PlaceID
    | GetAutocompleteSuggestions
    | GetPlaceDetails
    | GetTimeZoneInfo 
    | GotAutocompleteSuggestions PlaceAutocompleteRequest (Result Http.Error PlaceAutocompleteResponse)
    | GotPlaceDetails PlaceDetailsRequest (Result Http.Error PlaceDetailsResponse)
    | GotTimeZoneInfo TimeZoneRequest (Result Http.Error TimeZoneResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        GotDob dob_ ->
            case model.horoscopeRequest of
                Nothing ->
                    ( { model | horoscopeRequest = Just { dob = Just dob_, loc = Nothing } }, Cmd.none )

                Just r ->
                    ( { model | horoscopeRequest = Just { r | dob = Just dob_ } }, Cmd.none )

        GotLoc loc_ ->
            case model.horoscopeRequest of
                Nothing ->
                    ( { model | horoscopeRequest = Just { dob = Nothing, loc = Just loc_ } }, Cmd.none )

                Just r ->
                    ( { model | horoscopeRequest = Just { r | loc = Just loc_ } }, Cmd.none )

        NewHoroscope ->
            ( { model | horoscopeRequest = Just defaultData }, Cmd.none )

        GetHoroscope ->
            ( { model | horoscopeResponse = Just Loading }, getHoroscopeData model )

        -- TODO: validate before submitting!
        GotHoroscope req result ->
            case result of
                Ok r ->
                    ( { model | horoscopeResponse = Just (Success req r), horoscopeAspects = Just (deriveAspects r) }, Cmd.none )

                Err _ ->
                    ( { model | horoscopeResponse = Just (Failure req) }, Cmd.none )

        UpdatedPlaceInput partialInput ->
            case model.autocompleteRequest of
                Nothing ->
                    ({ model | autocompleteRequest = Just <| initPlaceRequest model.autocompleteSessionToken partialInput }, Cmd.none)
                
                Just r ->
                    ({ model | autocompleteRequest = Just <| updatePlaceRequest r partialInput}, Cmd.none)

        UpdatedTimeInput partialTimeInput ->
            case parseTime partialTimeInput of
                Ok posixTime ->
                    case model.placeDetailsResponse of
                        Nothing ->
                            ({model | partialTimeInput = Just partialTimeInput}, Cmd.none)

                        Just resp ->
                            (buildTimeZoneRequest model posixTime, Cmd.none)
                Err _ ->
                    ({ model | partialTimeInput = Just partialTimeInput }, Cmd.none)

        PlaceSelected p ->
            case model.placeDetailsRequest of
                Nothing ->
                    ({ model | placeDetailsRequest = Just <| initDetailsRequest model.autocompleteSessionToken p}, Cmd.none)

                Just r ->
                    ({ model | placeDetailsRequest = Just <| updateDetailsRequest r p}, Cmd.none)

        GetAutocompleteSuggestions ->
            ({ model | autocompleteResponse = Just Loading}, getAutocompleteSuggestions model)

        GetPlaceDetails ->
            ({model | placeDetailsResponse = Just Loading}, getPlaceDetails model)

        GetTimeZoneInfo ->
            ({model | timeZoneResponse = Just Loading}, getTimeZoneInfo model)

        GotAutocompleteSuggestions req resp ->
            case resp of
                Ok r ->
                    ( {model | autocompleteResponse = Just (Success req r)}, Cmd.none)
                Err _ ->
                    ( {model | autocompleteResponse = Just (Failure req)}, Cmd.none)
        
        GotPlaceDetails req resp ->
            let
                -- making this request marks the end of an autocomplete "session"
                (newToken, newSeed) = generateSessionToken model.tokenSeed
            in
            case resp of
                Ok r ->
                    ( {model | placeDetailsResponse = Just (Success req r), tokenSeed = newSeed, autocompleteSessionToken = SessionToken newToken}, Cmd.none)
                Err _ ->
                    ( {model | placeDetailsResponse = Just (Failure req), tokenSeed = newSeed, autocompleteSessionToken = SessionToken newToken}, Cmd.none)

        GotTimeZoneInfo req resp ->
            case resp of
                Ok r ->
                    ( {model | timeZoneResponse = Just (Success req r)}, Cmd.none)
                Err _ ->
                    ( {model | timeZoneResponse = Just (Failure req)}, Cmd.none)


        

parseTime : String -> Result String Time.Posix
parseTime maybeTime =
    case ISO.toTime maybeTime of
        Ok t -> Ok t
        Err _ -> Err "Invalid timestamp"

buildTimeZoneRequest : Model -> Time.Posix -> Model
buildTimeZoneRequest m t =
    case m.placeDetailsResponse of
        Nothing -> m
        Just placeDetailsResponse ->
            case placeDetailsResponse of
                Success _ data -> 
                    { m | timeZoneRequest = Just <| initTimeZoneRequest data.result t}
                _ -> m


deriveAspects : HoroscopeResponse -> AspectsList
deriveAspects { houseCusps, planetaryPositions } =
    defaultAspects <| combinePositions houseCusps planetaryPositions


combinePositions : List HouseCusp -> List PlanetPosition -> List Ecliptic
combinePositions hs ps =
    let
        pl =
            ps |> filterPlanets defaultPlanets |> List.map PlanetLocation

        hl =
            hs |> filterHouses defaultHouses |> List.map CuspLocation
    in
    List.append pl hl


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet -- TODO: remove for prod
        , div []
            [ menu model
            , Grid.container []
                [ Grid.row []
                    [ Grid.col []
                        [ viewRequestForm model
                        , viewChart model
                        ]
                    ]
                ]
            ]
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ Attrs.href "#" ] [ Html.text "Senex" ]
        |> Navbar.view model.navbarState


viewRequestForm : Model -> Html Msg
viewRequestForm { horoscopeRequest, horoscopeResponse } =
    case horoscopeRequest of
        Nothing ->
            div [] []

        Just r ->
            div []
                [ input [ Attrs.type_ "text", placeholder "Date of Birth", onInput GotDob, value (Maybe.withDefault "" r.dob) ] []
                , input [ Attrs.type_ "text", placeholder "Location (lat, long)", onInput GotLoc, value (Maybe.withDefault "" r.loc) ] []
                , button [ Evts.onClick GetHoroscope ] [ Html.text "Show Chart" ]
                ]


viewChart : Model -> Html Msg
viewChart { horoscopeRequest, horoscopeResponse, horoscopeAspects } =
    case horoscopeResponse of
        Nothing ->
            div [] [ Html.text "Enter your info to see your chart!" ]

        Just fetchData ->
            case fetchData of
                Loading ->
                    div [] [ Spinner.spinner [ Spinner.color Text.info ] [] ]

                Failure r ->
                    div []
                        [ Html.text "Unable to load data"
                        , button [ Evts.onClick GetHoroscope ] [ Html.text "Try Again" ]
                        ]

                Success entered data ->
                    Grid.container []
                        [ --button [ Evts.onClick NewHoroscope ] [ Html.text "New Horoscope" ]
                          -- Grid.row [] [ Grid.col [] [ requestHeading entered ] ]
                          Grid.row [] [ Grid.col [] [] ]
                        , Grid.row []
                            [ -- aspectsFilter <| aspectsList horoscopeAspects
                              Grid.col [] [ chart data (aspectsList horoscopeAspects) ]
                            , Grid.col [ Col.middleXs ] [ astroDataTables data ]
                            ]
                        , Grid.row [] [ Grid.col [] [ aspectsTable (aspectsList horoscopeAspects) (combinePositions data.houseCusps data.planetaryPositions) ] ]
                        ]


aspectsList : Maybe AspectsList -> AspectsList
aspectsList mx =
    case mx of
        Nothing ->
            []

        Just l ->
            l


requestHeading : HoroscopeRequest -> Html Msg
requestHeading { dob, loc } =
    h2 []
        [ Html.text
            ("Natal Chart for "
                ++ Maybe.withDefault "" dob
                ++ " in "
                ++ Maybe.withDefault "" loc
            )
        ]


astroDataTables : HoroscopeResponse -> Html Msg
astroDataTables { houseCusps, planetaryPositions } =
    div []
        [ housesTable houseCusps

        -- , planetsTable planetaryPositions
        ]


bodyName : EclipticArchon -> String
bodyName a =
    case a of
        PlanetArchon x ->
            planetText x

        HouseArchon y ->
            houseText y


archonMarkup : EclipticArchon -> Html Msg
archonMarkup a =
    case a of
        PlanetArchon x ->
            planetMarkup x

        HouseArchon y ->
            Html.text <| houseText y


planetMarkup : Planet -> Html Msg
planetMarkup p =
    case p of
        Lilith ->
            externalSvg "Lilith.svg#svg1" 15

        Chiron ->
            externalSvg "Chiron.svg#svg2" 15

        _ ->
            Html.text <| planetText p


aspectMarkup : HoroscopeAspect -> Html Msg
aspectMarkup { aspect, bodies, angle, orb } =
    let
        name_ =
            case aspect.name of
                Conjunction ->
                    Html.text "☌"

                Sextile ->
                    externalSvg "Sextile.svg#svg1" 15

                Square ->
                    Html.text "□"

                Trine ->
                    Html.text "△"

                Opposition ->
                    Html.text "☍"

                SemiSquare ->
                    Html.text "∠"

                Sesquisquare ->
                    Html.text "□∠"

                -- TODO: get an SVG
                SemiSextile ->
                    externalSvg "SemiSextile.svg#svg1" 15

                Quincunx ->
                    externalSvg "Quincunx.svg#svg1" 15

                Quintile ->
                    Html.text "Q"

                BiQuintile ->
                    Html.text "bQ"

                _ ->
                    Html.text ""
    in
    -- TODO: more info here!
    Html.span
        [ Attrs.title <| String.fromFloat angle
        , Attrs.style "color" <| Color.toCssString <| aspectColor aspect
        ]
        [ name_
        , Html.text " "
        , Html.text <| String.fromInt <| round orb
        ]


findAspect : ( EclipticArchon, EclipticArchon ) -> List (Maybe HoroscopeAspect) -> Maybe HoroscopeAspect
findAspect aspect l =
    let
        toArchon : Ecliptic -> EclipticArchon
        toArchon e =
            case e of
                CuspLocation h ->
                    HouseArchon h.house

                PlanetLocation p ->
                    PlanetArchon p.planet

        compareAspects : ( EclipticArchon, EclipticArchon ) -> ( EclipticArchon, EclipticArchon ) -> Bool
        compareAspects ( a, b ) ( x, y ) =
            ( a, b ) == ( y, x ) || ( a, b ) == ( y, x )

        isAspect : ( EclipticArchon, EclipticArchon ) -> Maybe HoroscopeAspect -> Bool
        isAspect ( a, b ) hAspect =
            case hAspect of
                Nothing ->
                    False

                Just xy ->
                    compareAspects ( a, b ) (Tuple.mapBoth toArchon toArchon xy.bodies)
    in
    List.filter (isAspect aspect) l
        |> List.head
        |> Maybe.withDefault Nothing


findArchon : EclipticArchon -> List Ecliptic -> Maybe Ecliptic
findArchon a l =
    let
        compareHouse h x =
            case x of
                CuspLocation y ->
                    y.house == h

                PlanetLocation z ->
                    False

        comparePlanet p x =
            case x of
                PlanetLocation y ->
                    y.planet == p

                CuspLocation y ->
                    False

        finderFn =
            case a of
                HouseArchon house ->
                    compareHouse house

                PlanetArchon planet ->
                    comparePlanet planet
    in
    List.filter finderFn l
        |> List.head


archonLongitude : EclipticArchon -> List Ecliptic -> Html Msg
archonLongitude n ps_ =
    let
        lng =
            findArchon n ps_

        txt =
            case lng of
                Nothing ->
                    ""

                Just pos ->
                    pos |> getEclipticLocation |> longitudeText
    in
    Html.text <| txt ++ " "


aspectsTable : AspectsList -> List Ecliptic -> Html Msg
aspectsTable as_ ps_ =
    let
        allArchons : List EclipticArchon
        allArchons =
            List.append (List.map PlanetArchon defaultPlanets) (List.map HouseArchon defaultHouses)

        aspectCell : Html Msg -> Html Msg
        aspectCell ha =
            td [ Attrs.style "border" "1px solid #dee2e6" ] [ ha ]

        aspectRow : EclipticArchon -> Html Msg
        aspectRow n =
            let
                filteredArchons =
                    takeWhile (not << (==) n) allArchons

                getAspectWith : EclipticArchon -> Html Msg
                getAspectWith o =
                    if n == o then
                        archonMarkup n

                    else
                        case findAspect ( n, o ) as_ of
                            Nothing ->
                                Html.text ""

                            Just ao ->
                                aspectMarkup ao
            in
            List.concat
                [ [ td
                        [ Attrs.style "border" "1px solid #dee2e6" ]
                        [ archonLongitude n ps_
                        , archonMarkup n
                        ]
                  ]
                , List.map (getAspectWith >> aspectCell) filteredArchons
                , [ td [ Attrs.style "background-color" "white" ] [ archonMarkup n ] ]
                ]
                |> tr []
    in
    table [ Attrs.style "width" "100%", Attrs.class "table-striped", Attrs.class "table-hover" ]
        [ tbody []
            (List.map aspectRow allArchons)
        ]


planetsTable : List PlanetPosition -> Html Msg
planetsTable positions =
    Table.table
        { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ Html.text "Planet" ]
                , Table.th [] [ Html.text "Longitude" ]
                ]
        , tbody = Table.tbody [] <| List.map planetRow <| filterPlanets defaultPlanets positions
        }


planetRow : PlanetPosition -> Table.Row Msg
planetRow { planet, position } =
    Table.tr []
        [ Table.td [] [ Html.text <| Debug.toString planet ++ " (" ++ planetText planet ++ ")" ]
        , Table.td [ Table.cellAttr <| Attrs.title (String.fromFloat position.long) ] [ Html.text <| longitudeText position.long ]
        ]


houseRow : HouseCusp -> Table.Row Msg
houseRow { house, cusp } =
    Table.tr []
        [ Table.td [] [ Html.text <| Debug.toString house ]
        , Table.td [ Table.cellAttr <| Attrs.title <| String.fromFloat cusp ] [ Html.text <| longitudeText cusp ]
        ]


cuspValue : HouseCusp -> Int
cuspValue hc =
    case hc.house of
        I ->
            1

        II ->
            2

        III ->
            3

        IV ->
            4

        V ->
            5

        VI ->
            6

        VII ->
            7

        VIII ->
            8

        IX ->
            9

        X ->
            10

        XI ->
            11

        XII ->
            12

        UnknownCusp ->
            13


housesTable : List HouseCusp -> Html Msg
housesTable cusps =
    let
        sortCusps =
            List.sortBy cuspValue
    in
    Table.table
        { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ Html.text "House" ]
                , Table.th [] [ Html.text "Longitude" ]
                ]
        , tbody = Table.tbody [] <| List.map houseRow <| sortCusps cusps
        }



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
    | TrueNode -- see: https://www.astro.com/swisseph/swisseph.htm#_Toc19109030
    | Lilith -- the `MeanApog`: see: https://www.astro.com/astrology/in_lilith_e.htm and https://www.astro.com/swisseph/swisseph.htm#_Toc19109029
    | OscuApog -- aka "Astrological True Lilith": https://www.astro.com/swisseph/swisseph.htm#_Toc19109031
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
getHoroscopeData { horoscopeRequest, horoscopeResponse } =
    case horoscopeRequest of
        Just r ->
            Http.post
                { url = "http://localhost:3030/api/horoscope"
                , expect = Http.expectJson (GotHoroscope r) horoscopeDecoder
                , body = encodeHoroscopeRequest r |> Http.jsonBody
                }

        Nothing ->
            Cmd.none

getAutocompleteSuggestions : Model -> Cmd Msg
getAutocompleteSuggestions { autocompleteRequest } =
    case autocompleteRequest of
        Nothing -> Cmd.none
        Just r ->
            Http.get
                { url = buildGoogleApiUrl <| PlaceAutocomplete r
                , expect = Http.expectJson (GotAutocompleteSuggestions r) autocompleteResponseDecoder
                }
            
getPlaceDetails : Model -> Cmd Msg
getPlaceDetails { placeDetailsRequest } =
    case placeDetailsRequest of
        Nothing -> Cmd.none
        Just r ->
            Http.get
                { url = buildGoogleApiUrl <| PlaceDetails r
                , expect = Http.expectJson (GotPlaceDetails r) placeDetailsDecoder
                }

getTimeZoneInfo : Model -> Cmd Msg
getTimeZoneInfo { timeZoneRequest } =
    case timeZoneRequest of
        Nothing -> Cmd.none
        Just r ->
            Http.get
                { url = buildGoogleApiUrl <| TimeZone r
                , expect = Http.expectJson (GotTimeZoneInfo r) timeZoneDecoder
                }
            
            

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
                    succeed Lilith

                "OscuApog" ->
                    succeed OscuApog

                "Earth" ->
                    succeed Earth_

                "Chiron" ->
                    succeed Chiron

                _ ->
                    succeed UnknownPlanet
    in
    Decode.string |> Decode.andThen planetHelp



-- | Chart drawing and associated types
-- Both cusps and planets can be placed on the ecliptic, but they have
-- different coordinate information.


type Ecliptic
    = CuspLocation HouseCusp
    | PlanetLocation PlanetPosition


type EclipticArchon
    = HouseArchon House
    | PlanetArchon Planet


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


type AspectTemperament
    = Analytical -- "Disharmonious"
    | Synthetic -- "Harmonious"
    | Neutral


type alias Aspect =
    { name : AspectName, maxOrb : Float, angle : Float, temperament : AspectTemperament }


type alias HoroscopeAspect =
    { aspect : Aspect
    , bodies : ( Ecliptic, Ecliptic )
    , angle : Float
    , orb : Float
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
    [ { name = Conjunction, angle = 0.0, maxOrb = 10.0, temperament = Synthetic }
    , { name = Sextile, angle = 60.0, maxOrb = 6.0, temperament = Synthetic }
    , { name = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
    , { name = Trine, angle = 120.0, maxOrb = 10.0, temperament = Synthetic }
    , { name = Opposition, angle = 180.0, maxOrb = 10.0, temperament = Analytical }
    ]


minorAspects : List Aspect
minorAspects =
    [ { name = SemiSquare, angle = 45.0, maxOrb = 3.0, temperament = Analytical }
    , { name = Sesquisquare, angle = 135.0, maxOrb = 3.0, temperament = Analytical }
    , { name = SemiSextile, angle = 30.0, maxOrb = 3.0, temperament = Neutral }
    , { name = Quincunx, angle = 150.0, maxOrb = 3.0, temperament = Neutral }
    , { name = Quintile, angle = 72.0, maxOrb = 2.0, temperament = Synthetic }
    , { name = BiQuintile, angle = 144.0, maxOrb = 2.0, temperament = Synthetic }
    ]


westernSigns : List ZodiacSign
westernSigns =
    [ { name = Aries, longitude = 0.0, element = Fire }
    , { name = Taurus, longitude = 30.0, element = Earth }
    , { name = Gemini, longitude = 60.0, element = Air }
    , { name = Cancer, longitude = 90.0, element = Water }
    , { name = Leo, longitude = 120.0, element = Fire }
    , { name = Virgo, longitude = 150.0, element = Earth }
    , { name = Libra, longitude = 180.0, element = Air }
    , { name = Scorpio, longitude = 210.0, element = Water }
    , { name = Sagittarius, longitude = 240.0, element = Fire }
    , { name = Capricorn, longitude = 270.0, element = Earth }
    , { name = Aquarius, longitude = 300.0, element = Air }
    , { name = Pisces, longitude = 330.0, element = Water }
    ]


defaultPlanets : List Planet
defaultPlanets =
    [ Sun
    , Moon
    , Mercury
    , Venus
    , Mars
    , Jupiter
    , Saturn
    , Uranus
    , Neptune
    , Pluto
    , Chiron
    , MeanNode
    , Lilith
    ]


defaultHouses : List House
defaultHouses =
    [ I, X ]


filterPlanets : List Planet -> List PlanetPosition -> List PlanetPosition
filterPlanets planetsToKeep planetPositions =
    let
        keepPlanet : PlanetPosition -> Bool
        keepPlanet p =
            List.member p.planet planetsToKeep
    in
    List.filter keepPlanet planetPositions


filterHouses : List House -> List HouseCusp -> List HouseCusp
filterHouses housesToKeep houseCusps =
    let
        keepHouse : HouseCusp -> Bool
        keepHouse h =
            List.member h.house housesToKeep
    in
    List.filter keepHouse houseCusps


houseAngle : House -> List HouseCusp -> Maybe Float
houseAngle h cusps =
    let
        isAngle : House -> HouseCusp -> Maybe HouseCusp
        isAngle h_ c_ =
            case c_.house == h_ of
                True ->
                    Just c_

                False ->
                    Nothing
    in
    List.filterMap (isAngle h) cusps
        |> List.head
        |> Maybe.andThen (\c -> Just c.cusp)


ascendantAngle =
    houseAngle I


getEclipticLocation : Ecliptic -> Float
getEclipticLocation eclipticBody =
    case eclipticBody of
        PlanetLocation p ->
            p.position.long

        CuspLocation c ->
            c.cusp


haveAspect : Ecliptic -> Ecliptic -> Aspect -> Maybe HoroscopeAspect
haveAspect a b aspect =
    let
        angle =
            getEclipticLocation a - getEclipticLocation b

        counterAngle =
            360 - abs angle

        orbCalc =
            \x -> aspect.angle - abs x

        angles =
            List.filter (\x -> (x |> orbCalc |> abs) <= aspect.maxOrb) [ angle, counterAngle ]
    in
    case angles of
        [] ->
            Nothing

        x :: xs ->
            Just { aspect = aspect, bodies = ( a, b ), angle = x, orb = orbCalc x }


aspectsBetween : List Aspect -> ( Ecliptic, Ecliptic ) -> List (Maybe HoroscopeAspect)
aspectsBetween possibleAspects ( planetA, planetB ) =
    let
        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    List.map (haveAspect planetA planetB) possibleAspects
        |> List.filter isJust



-- TODO: prune redundant aspects (aspects which are multiples of one another)
-- sorting by angle, and then removing duplicate (Ecliptic, Ecliptic) pairs.


calculateAspects : List Aspect -> List Ecliptic -> List (Maybe HoroscopeAspect)
calculateAspects aspectList planetPositions =
    planetPositions
        |> uniquePairs
        |> List.concatMap (aspectsBetween aspectList)


defaultAspects : List Ecliptic -> List (Maybe HoroscopeAspect)
defaultAspects =
    calculateAspects (List.append majorAspects minorAspects)


chart : HoroscopeResponse -> AspectsList -> Html Msg
chart { houseCusps, planetaryPositions } calculatedAspects =
    let
        width =
            666

        center =
            width / 2

        r =
            width * 0.42

        z =
            33.0

        -- zodiac band thickness
        o =
            ascendantAngle houseCusps |> Maybe.withDefault 0.0

        container =
            { centerX = center, centerY = center, radius = r, offset = 180 - o }

        housesOuterCircle =
            { container | radius = container.radius - z }

        aspectsOuterCircle =
            { container | radius = container.radius - 3 * z }
    in
    svg
        [ SvgAttrs.width (String.fromFloat width), SvgAttrs.height (String.fromFloat width) ]
        [ g [ SvgAttrs.id "radix" ]
            [ zodiac container housesOuterCircle
            , houses housesOuterCircle aspectsOuterCircle houseCusps
            , planets container planetaryPositions
            , aspects aspectsOuterCircle calculatedAspects
            ]
        ]


aspectsCircle =
    housesCircle


aspects : Circle -> List (Maybe HoroscopeAspect) -> Svg Msg
aspects container aspectsData =
    g [ SvgAttrs.id "aspectsCircle" ]
        [ aspectsCircle container
        , g [ SvgAttrs.id "aspects" ] (drawAspects container aspectsData)
        ]


zodiac : Circle -> Circle -> Svg Msg
zodiac outer inner =
    g [ SvgAttrs.id "zodiac" ]
        [ zodiacCircle outer
        , g [ SvgAttrs.id "signs" ] (zodiacSigns outer inner)
        ]


houses : Circle -> Circle -> List HouseCusp -> Svg Msg
houses outer inner housesData =
    g [ SvgAttrs.id "housesCircle" ]
        [ housesCircle outer
        , g [ SvgAttrs.id "houses" ] (drawHouses outer inner housesData)
        ]


planets : Circle -> List PlanetPosition -> Svg Msg
planets containerCircle planetsData =
    g [ SvgAttrs.id "planetsCircle" ]
        [ g [ SvgAttrs.id "planets" ] (drawPlanets containerCircle planetsData)
        ]


housesCircle : Circle -> Svg Msg
housesCircle { centerX, centerY, radius } =
    circle
        [ cx (String.fromFloat centerX)
        , cy (String.fromFloat centerY)
        , r (String.fromFloat radius)
        , fill "none"
        , stroke "#444"
        , strokeWidth "1"
        ]
        []


drawPlanets : Circle -> List PlanetPosition -> List (Svg Msg)
drawPlanets c d =
    List.map (drawPlanet c) d


drawHouses : Circle -> Circle -> List HouseCusp -> List (Svg Msg)
drawHouses c c_ d =
    List.map (drawHouse c c_) d


drawAspects : Circle -> List (Maybe HoroscopeAspect) -> List (Svg Msg)
drawAspects c d =
    let
        -- TODO: promote this to a `dropNils` fn??
        aspectFolder : Maybe HoroscopeAspect -> List HoroscopeAspect -> List HoroscopeAspect
        aspectFolder x acc =
            case x of
                Nothing ->
                    acc

                Just a ->
                    a :: acc
    in
    List.foldl aspectFolder [] d
        |> List.map (drawAspect c)


drawAspect : Circle -> HoroscopeAspect -> Svg Msg
drawAspect container aspect =
    let
        ( a, b ) =
            aspect.bodies

        longitudeA =
            a |> getEclipticLocation |> negate

        longitudeB =
            b |> getEclipticLocation |> negate
    in
    g []
        [ Svg.path
            [ d (lineBetweenAngles container longitudeA longitudeB)
            , fill "none"
            , strokeWidth "3"
            , stroke (Color.toCssString <| aspectColor aspect.aspect)
            ]
            []
        ]



-- See: https://www.astro.com/faq/fq_fh_design_e.htm


aspectColor : Aspect -> Color
aspectColor a =
    case a.temperament of
        Analytical ->
            Color.darkRed

        Synthetic ->
            Color.darkBlue

        Neutral ->
            Color.lightGreen



{-
   drawDegrees : Circle -> List Int -> List (Svg Msg)
   drawDegrees c d = List.map (drawDegree c) d

   drawDegree : Circle -> Int -> Svg Msg
   drawDegree container dg =
     g []
       [ Svg.path [d (drawLinePath container (toFloat dg) (0.128*1.5)), fill "none", strokeWidth "1", stroke (Color.toCssString Color.lightGray)] []
       , drawTextAtDegree container (String.fromInt dg)  "font: italic 2px serif; fill: #222;" -(toFloat dg)
       ]
-}


houseLineLength : House -> Circle -> Circle -> Float
houseLineLength house outer inner =
    case List.member house [ I, IV, VII, X ] of
        True ->
            outer.radius

        False ->
            outer.radius - inner.radius


drawHouse : Circle -> Circle -> HouseCusp -> Svg Msg
drawHouse outer inner { house, cusp } =
    let
        houseTextStyle h =
            -- show a more prominent text for the Ascendant and MC:
            case List.member h [ I, X ] of
                True ->
                    "font: 10px sans-serif; fill: " ++ Color.toCssString Color.darkCharcoal ++ ";"

                False ->
                    "font: italic 10px serif; fill: " ++ Color.toCssString Color.darkGreen ++ ";"
    in
    g [ SvgAttrs.id (Debug.toString house) ]
        [ Svg.path [ d (drawRadialLine outer -cusp (houseLineLength house outer inner)), fill "none", strokeWidth "2", stroke (Color.toCssString Color.lightCharcoal) ] []
        , drawTextAtDegree outer (houseText house) (houseTextStyle house) -(cusp + 5)
        ]


drawPlanet container planetPosition =
    let
        drawing =
            case List.member planetPosition.planet [ Chiron, Lilith ] of
                True ->
                    planetGlyph container planetPosition

                False ->
                    drawTextAtDegree container (planetText planetPosition.planet) "font: 15px sans-serif; fill: black;" -planetPosition.position.long
    in
    g [] [ drawing ]


zodiacCircle : Circle -> Svg Msg
zodiacCircle { centerX, centerY, radius } =
    circle [ cx (String.fromFloat centerX), cy (String.fromFloat centerY), r (String.fromFloat radius), fill "none", stroke "#333", strokeWidth "2" ] []


zodiacSigns : Circle -> Circle -> List (Svg Msg)
zodiacSigns i o =
    List.map (zodiacSign i o) westernSigns


zodiacSign : Circle -> Circle -> ZodiacSign -> Svg Msg
zodiacSign outer inner sign =
    let
        signLengthInDegrees =
            30.0

        signBandThickness =
            outer.radius - inner.radius
    in
    g []
        [ Svg.path [ d (buildSlicePath outer signLengthInDegrees signBandThickness -sign.longitude), fill (elementColor sign.element), strokeWidth "0", stroke "none" ] []
        , signGlyph
            outer
            { sign | longitude = -(sign.longitude + (signLengthInDegrees / 2)) }
        ]


longitudeText : Angle -> String
longitudeText a =
    let
        aLong : AstrologicalLongitude
        aLong =
            getAstrologicalLongitude westernSigns a

        signText =
            case aLong.sign of
                Just z ->
                    zodiacText z

                Nothing ->
                    ""

        degComponent : String -> Int -> String
        degComponent suffix =
            String.fromInt >> String.padLeft 2 '0' >> (\x -> x ++ suffix)

        dText =
            aLong.degrees |> degComponent "°"

        mText =
            aLong.minutes |> degComponent "′"

        sText =
            aLong.seconds |> degComponent "″"
    in
    case aLong.sign of
        -- couldn't calculate it relative to a constellation
        Nothing ->
            aLong.angle |> String.fromFloat

        Just _ ->
            signText ++ " " ++ dText ++ mText ++ sText


planetText : Planet -> String
planetText p =
    case p of
        Sun ->
            "☉"

        Moon ->
            "☽"

        Mercury ->
            "☿"

        Venus ->
            "♀︎"

        Earth_ ->
            ""

        Mars ->
            "♂︎"

        Jupiter ->
            "♃"

        Saturn ->
            "♄"

        Uranus ->
            "♅"

        Neptune ->
            "♆"

        Pluto ->
            "♇"

        MeanNode ->
            "☊"

        Lilith ->
            "Lilith"

        Chiron ->
            "Chiron"

        _ ->
            ""


zodiacText : ZodiacSignName -> String
zodiacText z =
    case z of
        Aries ->
            "♈️"

        Taurus ->
            "♉️"

        Gemini ->
            "♊️"

        Cancer ->
            "♋️"

        Leo ->
            "♌️"

        Virgo ->
            "♍️"

        Libra ->
            "♎️"

        Scorpio ->
            "♏️"

        Sagittarius ->
            "♐️"

        Capricorn ->
            "♑️"

        Aquarius ->
            "♒️"

        Pisces ->
            "♓️"


houseText : House -> String
houseText h =
    case h of
        I ->
            "ASC"

        II ->
            "2"

        III ->
            "3"

        IV ->
            "4"

        V ->
            "5"

        VI ->
            "6"

        VII ->
            "7"

        VIII ->
            "8"

        IX ->
            "9"

        X ->
            "MC"

        XI ->
            "11"

        XII ->
            "12"

        UnknownCusp ->
            ""



-- | Helper functions for the crazy math
-- TODO: this is a better abstraction than "polar", and it'd help us to not have to negate
-- longitudes everywhere!
-- https://en.wikipedia.org/wiki/Ecliptic_coordinate_system


type alias Circle =
    { centerX : Float, centerY : Float, radius : Float, offset : Float }


type alias Angle =
    Float


type alias Cartesian =
    { x : Float, y : Float }


type alias AstrologicalLongitude =
    { sign : Maybe ZodiacSignName -- there's a chance it won't be found
    , angle : Angle -- original angle
    , degrees : Int
    , minutes : Int
    , seconds : Int
    }


getAstrologicalLongitude : List ZodiacSign -> Angle -> AstrologicalLongitude
getAstrologicalLongitude signList decimalAngle =
    let
        d =
            truncate decimalAngle

        m =
            (decimalAngle - toFloat d) * 60.0 |> truncate

        s =
            (decimalAngle - toFloat d - (toFloat m / 60.0)) * 3600.0 |> round

        closestSignLongitude =
            (d // 30) * 30 |> toFloat

        zs =
            List.filter (.longitude >> (==) closestSignLongitude) signList
                |> List.head
                |> Maybe.andThen (.name >> Just)

        relativeDegrees =
            (closestSignLongitude - toFloat d) |> abs |> truncate
    in
    { sign = zs, angle = decimalAngle, degrees = relativeDegrees, minutes = m, seconds = s }


polarToCartesian : Circle -> Angle -> Cartesian
polarToCartesian { centerX, centerY, radius, offset } angle =
    let
        -- all coordinates should be relative to the offset, which is the angle
        -- between the Ascendant and 180 degrees from the zero point of the container
        -- (which isn't really Aries, as Aries will also be off-set.)
        offsetAngle =
            angle - offset

        ( x_, y_ ) =
            fromPolar ( radius, degrees offsetAngle )
    in
    { x = centerX + x_, y = centerY + y_ }


elementColor : ClassicalElement -> String
elementColor element =
    let
        color =
            case element of
                Earth ->
                    Color.darkGreen

                Air ->
                    Color.yellow

                Fire ->
                    Color.lightRed

                Water ->
                    Color.lightBlue
    in
    Color.toCssString color


lineBetweenAngles : Circle -> Float -> Float -> String
lineBetweenAngles container startAngle endAngle =
    let
        start =
            polarToCartesian container startAngle

        end =
            polarToCartesian container endAngle

        elements =
            [ "M"
            , String.fromFloat start.x
            , String.fromFloat start.y
            , "L"
            , String.fromFloat end.x
            , String.fromFloat end.y
            ]
    in
    String.join " " elements



-- from: https://stackoverflow.com/a/43211655


buildSlicePath : Circle -> Float -> Float -> Float -> String
buildSlicePath containerCircle length thickness longitude =
    let
        endAngle =
            longitude

        startAngle =
            longitude - length

        innerCircle =
            { containerCircle | radius = containerCircle.radius - thickness }

        innerStart =
            polarToCartesian innerCircle endAngle

        innerEnd =
            polarToCartesian innerCircle startAngle

        outerStart =
            polarToCartesian containerCircle endAngle

        outerEnd =
            polarToCartesian containerCircle startAngle

        largeArcFlag =
            if endAngle >= startAngle then
                if endAngle - startAngle <= 180.0 then
                    "0"

                else
                    "1"

            else if (endAngle + 360.0) - startAngle <= 180.0 then
                "0"

            else
                "1"

        elements =
            [ "M"
            , String.fromFloat outerStart.x
            , String.fromFloat outerStart.y
            , "A"
            , String.fromFloat containerCircle.radius
            , String.fromFloat containerCircle.radius
            , "0"
            , largeArcFlag
            , "0"
            , String.fromFloat outerEnd.x
            , String.fromFloat outerEnd.y
            , "L"
            , String.fromFloat innerEnd.x
            , String.fromFloat innerEnd.y
            , "A"
            , String.fromFloat innerCircle.radius
            , String.fromFloat innerCircle.radius
            , "0"
            , largeArcFlag
            , "1"
            , String.fromFloat innerStart.x
            , String.fromFloat innerStart.y
            , "L"
            , String.fromFloat outerStart.x
            , String.fromFloat outerStart.y
            , "Z"
            ]
    in
    String.join " " elements


drawRadialLine : Circle -> Float -> Float -> String
drawRadialLine containerCircle longitude lineLength =
    let
        startAngle =
            longitude

        innerCircle =
            { containerCircle | radius = containerCircle.radius - lineLength }

        innerEnd =
            polarToCartesian innerCircle startAngle

        outerEnd =
            polarToCartesian containerCircle startAngle

        elements =
            [ "M"
            , String.fromFloat outerEnd.x
            , String.fromFloat outerEnd.y
            , "L"
            , String.fromFloat innerEnd.x
            , String.fromFloat innerEnd.y
            ]
    in
    String.join " " elements


drawTextAtDegree : Circle -> String -> String -> Float -> Svg Msg
drawTextAtDegree containerCircle text style longitude =
    let
        spread =
            containerCircle.radius * (0.128 * 1.5)

        loc =
            polarToCartesian { containerCircle | radius = containerCircle.radius - spread } longitude
    in
    Svg.text_ [ SvgAttrs.x (String.fromFloat loc.x), SvgAttrs.y (String.fromFloat loc.y), SvgAttrs.style style ]
        [ Svg.text text ]


signGlyph : Circle -> ZodiacSign -> Svg Msg
signGlyph container { name, longitude, element } =
    let
        inner =
            { container | radius = container.radius - 18.0 }

        size =
            20.0

        svgName =
            case name of
                Aries ->
                    "Aries.svg#svg602"

                Taurus ->
                    "Taurus.svg#svg614"

                Gemini ->
                    "Gemini.svg#svg620"

                Cancer ->
                    "Cancer.svg#svg626"

                Leo ->
                    "Leo.svg#svg632"

                Virgo ->
                    "Virgo.svg#svg638"

                Libra ->
                    "Libra.svg#svg644"

                Scorpio ->
                    "Scorpio.svg#svg650"

                Sagittarius ->
                    "Sagittarius.svg#svg656"

                Capricorn ->
                    "Capricorn.svg#svg1"

                Aquarius ->
                    "Aquarius.svg#svg668"

                Pisces ->
                    "Pisces.svg#svg674"

        svgPath =
            "/assets/img/" ++ svgName
    in
    drawGlyphAtDegree inner longitude svgPath size


planetGlyph : Circle -> PlanetPosition -> Svg Msg
planetGlyph container { planet, position } =
    let
        inner =
            { container | radius = container.radius - 18.0 * 3.0 }

        size =
            15.0

        svgName =
            case planet of
                Sun ->
                    "Sun.svg#svg1"

                Moon ->
                    "Moon.svg#svg1"

                Mercury ->
                    "Mercury.svg#svg1"

                Venus ->
                    "Venus.svg#svg1"

                Mars ->
                    "Mars.svg#svg1"

                Jupiter ->
                    "Jupiter.svg#svg1"

                Saturn ->
                    "Saturn.svg#svg1"

                Uranus ->
                    "Uranus.svg#svg1"

                Neptune ->
                    "Neptune.svg#svg1"

                Pluto ->
                    "Pluto.svg#svg1"

                Lilith ->
                    "Lilith.svg#svg1"

                MeanNode ->
                    "MeanNode.svg#svg2"

                Chiron ->
                    "Chiron.svg#svg2"

                _ ->
                    ""

        -- TODO: handle this better?
        svgPath =
            "/assets/img/" ++ svgName
    in
    -- TODO: yet another weird negated angle, centralize this!
    drawGlyphAtDegree inner -position.long svgPath size


drawGlyphAtDegree : Circle -> Angle -> String -> Float -> Svg Msg
drawGlyphAtDegree circle angle svgPath size =
    let
        loc =
            polarToCartesian circle angle
    in
    Svg.use
        [ SvgAttrs.xlinkHref svgPath
        , SvgAttrs.width <| String.fromFloat size
        , SvgAttrs.height <| String.fromFloat size
        , SvgAttrs.x <| String.fromFloat (loc.x - size / 2)
        , SvgAttrs.y <| String.fromFloat (loc.y - size / 2)
        ]
        []


externalSvg : String -> Float -> Html Msg
externalSvg nameAndId size =
    let
        use_ =
            Svg.use
                [ SvgAttrs.xlinkHref ("/assets/img/" ++ nameAndId)
                , SvgAttrs.width <| String.fromFloat size
                , SvgAttrs.height <| String.fromFloat size
                ]
                []
    in
    Svg.svg
        [ SvgAttrs.width <| String.fromFloat size
        , SvgAttrs.height <| String.fromFloat size
        ]
        [ use_ ]



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