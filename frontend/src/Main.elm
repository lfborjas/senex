import Browser
import Http
--import Element exposing (..)
--import Element.Events exposing (..)
--import Element.Input exposing (..)
import Html exposing (Html, text, pre)


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

type Msg = GotData (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
        text "Unable to load data"
      Loading ->
        text "Loading..."
      NotStarted ->
        text "Not started"
      Success t ->
        Html.pre [] [ text t ]