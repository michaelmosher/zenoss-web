import Html exposing (Html, div, h2, p, text)
import Http
import Navigation
import UrlParser as Url exposing ((</>))

import LocalSettings
import Login
import Main.Model exposing (Msg(..), Model, Page(..), Setting)
import Zenoss.Http
import Zenoss.Html

main : Program Never Model Msg
main =
  Navigation.program UrlChange {
    init = init,
    view =  view,
    update =  update,
    subscriptions = subscriptions
  }

init: Navigation.Location -> (Model, Cmd Msg)
init _ = 
  (
      Model (Just LoginPage) "" "" "" [],
      LocalSettings.loadInitialSettings
  )


subscriptions: Model -> Sub Msg
subscriptions _ =
    LocalSettings.newSetting NewSetting


route: Url.Parser (Page -> a) a
route =
  Url.oneOf [
    Url.map LoginPage Url.top,
    Url.map EventsPage (Url.s "Events"),
    Url.map EventPage (Url.s "Event" </> Url.string)
  ]


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange l -> 
      ({model | currentPage = Url.parseHash route l}, Cmd.none)

    NewSetting s ->
      (LocalSettings.storeSetting s model, Cmd.none)

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
        model ! [
          cMsg, Navigation.newUrl "#Events",
          LocalSettings.setSetting {key = "Zhostname", value = Just model.hostname},
          LocalSettings.setSetting {key = "Zusername", value = Just model.username},
          LocalSettings.setSetting {key = "Zpassword", value = Just model.password}
        ]

    FetchEvents ->
      let
        auth = {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        }
      in
        (model, Http.send NewEvents (Zenoss.Http.queryEvents auth))

    NewEvents (Ok e) ->
      ({model | events = e}, Cmd.none)

    NewEvents (Err e) -> 
      let
        loginFailed = Main.Model.Event "" "zenoss-web"
                            ("Failed to connect to Zenoss server: " ++ toString e)
                            "Production" "Critical" Main.Model.New Nothing 1
                            "null" "null" Nothing "N/A"
      in
        ({model | events = [loginFailed]}, Cmd.none)

    EventDetails eid ->
        (model, "#Event/" ++ eid |> Navigation.newUrl)


view: Model -> Html Msg
view model =
  case model.currentPage of
    Just LoginPage -> 
      Login.pageView model

    Just EventsPage -> 
        div [] [
            h2 [] [text "Events!"],
            Zenoss.Html.renderEventList model.events
        ]
    
    Just (EventPage eid) ->
        div [] [
            h2 [] [text "One Event!"],
            Zenoss.Html.renderEventDetails model.events eid
        ]
    
    Nothing ->
      h2 [] [text "ERRORS!"]