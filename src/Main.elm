import Html exposing (Html, div, h2, p, text)
import Http
import Navigation
import UrlParser as Url

import Login
import Main.Model exposing (Msg(..), Model, Page(..))
import Zenoss
import Zenoss.Html

main : Program Never Model Msg
main =
  Navigation.program UrlChange {
    init = init,
    view =  view,
    update =  update,
    subscriptions = (\_ -> Sub.none)
  }

init: Navigation.Location -> (Model, Cmd Msg)
init _ = 
  (
      Model (Just LoginPage) "" "" "" [],
      Cmd.none
  )


route: Url.Parser (Page -> a) a
route =
  Url.oneOf [
    Url.map LoginPage Url.top,
    Url.map Events (Url.s "Events")
  ]


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange l -> 
      ({model | currentPage = Url.parseHash route l}, Cmd.none)

    UpdateHostname h ->
      ({model | hostname = h}, Cmd.none)

    UpdateUsername u ->
      ({model | username = u}, Cmd.none)

    UpdatePassword p ->
      ({model | password = p}, Cmd.none)

    LoginMsg ->
      let
        cMsg = Tuple.second (update FetchEvents model)
      in
        model ! [cMsg, Navigation.newUrl "#Events"]

    FetchEvents ->
      let
        auth = {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        }
      in
        (model, Http.send NewEvents (Zenoss.eventsRequest auth))

    NewEvents (Ok e) ->
      ({model | events = e}, Cmd.none)

    NewEvents (Err e) -> 
      let
        loginFailed = Main.Model.Event "" "zenoss-web"
                            ("Failed to connect to Zenoss server: " ++ toString e)
                            "Production" "Critical" Main.Model.New Nothing 1
      in
        ({model | events = [loginFailed]}, Cmd.none)


view: Model -> Html Msg
view model =
  case model.currentPage of
    Just LoginPage -> 
      Login.pageView

    Just Events -> 
        div [] [
            h2 [] [text "Events!"],
            Zenoss.Html.renderEventList model.events
        ]
    
    Nothing ->
      h2 [] [text "ERRORS!"]