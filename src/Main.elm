import Html exposing (Html, h2, text)
import Navigation
import UrlParser as Url exposing ((</>))

import Dashboard.Html
import LocalSettings
import Login
import Main.Html
import Main.Model exposing (Msg(..), Model, Page(..), Setting)
import Zenoss

type alias Auth = {
    hostname: String,
    username: String,
    password: String
}

main: Program Auth Model Msg
main =
    Navigation.programWithFlags UrlChange {
        init = init,
        view =  view,
        update =  update,
        subscriptions = subscriptions
    }

init: Auth -> Navigation.Location -> (Model, Cmd Msg)
init auth location =
    let
        loggedIn = auth.hostname /= ""
            && auth.username /= ""
            && auth.password /= ""
        initialPage = if loggedIn
            then Url.parseHash route location
            else Just LoginPage
        model = Model initialPage auth.hostname auth.username auth.password [] []
        initialAction = case initialPage of
            Just LoginPage -> LocalSettings.loadInitialSettings
            Just DevicesPage -> model |> Zenoss.refreshDevices
            _ -> model |> Zenoss.refreshEvents
    in
        (model, initialAction)


subscriptions: Model -> Sub Msg
subscriptions _ =
    LocalSettings.newSetting NewSetting


route: Url.Parser (Page -> a) a
route =
    Url.oneOf [
        Url.map EventsPage Url.top,
        Url.map DashboardPage (Url.s "Dashboard"),
        Url.map DevicesPage (Url.s "Devices"),
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
          model ! [
                Zenoss.refreshEvents model,
                Navigation.newUrl "#Events",
                LocalSettings.setSetting {key = "Zhostname", value = Just model.hostname},
                LocalSettings.setSetting {key = "Zusername", value = Just model.username},
                LocalSettings.setSetting {key = "Zpassword", value = Just model.password}
            ]

        RefreshEvents ->
            model ! [
                Navigation.newUrl "#Events",
                Zenoss.refreshEvents model
            ]

        NewEvents (Ok e) ->
            ({model | events = e}, Cmd.none)

        NewEvents (Err e) ->
            let loginFailed = "Failed to connect to Zenoss server: " ++ toString e
                    |> Zenoss.errorEvent
            in
                ({model | events = [loginFailed]}, Cmd.none)

        EventDetails eid ->
            (model, "#Event/" ++ eid |> Navigation.newUrl)

        AcknowledgeEvent eid ->
            Zenoss.acknowledgeEvent model eid

        AcknowledgeResponse eid (Ok e) ->
            (model, Cmd.none)

        AcknowledgeResponse eid (Err e) ->
            ({model | events = Zenoss.changeEventState model eid}, Cmd.none)

        UnacknowledgeEvent eid ->
            Zenoss.unacknowledgeEvent model eid

        UnacknowledgeResponse eid (Ok e) ->
            (model, Cmd.none)

        UnacknowledgeResponse eid (Err e) ->
            ({model | events = Zenoss.changeEventState model eid}, Cmd.none)

        RefreshDevices ->
            model ! [
                Navigation.newUrl "#Devices",
                Zenoss.refreshDevices model
            ]

        NewDevices (Ok d) ->
            ({model | devices = d}, Cmd.none)

        NewDevices (Err e) ->
            let loginFailed = "Failed to connect to Zenoss server: " ++ toString e
                    |> Zenoss.errorEvent
            in
                ({model | events = [loginFailed]}, Navigation.newUrl "#Events")

        ShowDashboard ->
            (model, Navigation.newUrl "#Dashboard")

view: Model -> Html Msg
view model =
    case model.currentPage of
        Just LoginPage ->
            Login.pageView model

        Just DashboardPage ->
             Main.Html.overlay [Dashboard.Html.renderDashboard] model.currentPage

        Just DevicesPage ->
            Main.Html.overlay [Zenoss.devicesView model] model.currentPage

        Just EventsPage ->
            Main.Html.overlay [Zenoss.eventsView model] model.currentPage

        Just (EventPage eid) ->
            Main.Html.overlay [Zenoss.eventDetailView model eid] model.currentPage

        Nothing ->
            h2 [] [text "ERRORS!"]
