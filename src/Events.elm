import Html exposing (Html, div, h2, p, span, input, button, text)
import Html.Attributes exposing (type_, style)
import Html.Events exposing (onInput, onClick)
import Http

import Zenoss
import Zenoss.Html

main: Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  {
    error: String,
    loggedIn: Bool,
    zUsername: String,
    zPassword: String,
    zHostname: String,
    events: List Zenoss.Event
  }

init : (Model, Cmd Msg)
init =
  (Model "none" False "" "" "" [], Cmd.none)


-- UPDATE
type Msg =
  UpdateUsername String
  | UpdatePassword String
  | UpdateHostname String
  | FetchEvents
  | NewEvents (Result Http.Error (List Zenoss.Event))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateUsername name ->
      ({model | zUsername = name}, Cmd.none)

    UpdatePassword pass ->
      ({model | zPassword = pass}, Cmd.none)

    UpdateHostname host ->
      ({model | zHostname = host}, Cmd.none)

    FetchEvents ->
      ({model | loggedIn = True}, fetchEvents model)

    NewEvents (Ok e) ->
      ({ model | events = e }, Cmd.none)

    NewEvents (Err e) ->
      ({ model | error = toString e}, Cmd.none)


fetchEvents: Model -> Cmd Msg
fetchEvents model =
  let
      eventRequest = Zenoss.eventsRequest {
        hostname = model.zHostname,
        username = model.zUsername,
        password = model.zPassword
      }
  in
      Http.send NewEvents eventRequest

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html.Html Msg
view model =
  let
      mainBody = if model.loggedIn
      then Zenoss.Html.renderEventList model.events
      else loginForm
  in
      
  div [] [
    h2 [] [text "Events"],
    p [] [text ("Errors: " ++ model.error)],
    mainBody
  ]


loginForm: Html.Html Msg
loginForm =
  div [] [
    div [style [("display", "flow")]] [
      p [style [("flex-grow", "1")]] [text "Zenoss Hostname: "],
      input [onInput UpdateHostname] []
    ],
    div [style [("display", "flow")]] [
      p [style [("flex-grow", "1")]] [text "Zenoss Username: "],
      input [onInput UpdateUsername] []
    ],
    div [style [("display", "flow")]] [
      p [style [("flex-grow", "1")]] [text "Zenoss Password: "],
      input [onInput UpdatePassword] []
    ],
    button [ onClick FetchEvents ] [ text "Fetch Events" ]
  ]
