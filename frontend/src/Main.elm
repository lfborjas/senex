import Browser
import Http
--import Element exposing (..)
--import Element.Events exposing (..)
--import Element.Input exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode

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
           | Success String

init : () -> (Model, Cmd Msg)
init _ = (NotStarted, Cmd.none)

type Msg = AskData 
         | GotData (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AskData -> 
      (Loading, getHoroscopeData)

    GotData result ->
      case result of
        Ok responseText ->
          (Success responseText, Cmd.none)
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
      Success t ->
        pre [] [ text t ]

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
    , expect = Http.expectString GotData
    , body = encodeHoroscopeRequest m |> Http.jsonBody
    }