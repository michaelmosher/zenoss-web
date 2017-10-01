module Zenoss exposing (..)

import Html exposing (Html, div, h2, text)
import Http
import Navigation

import Main.Model exposing (Model, Msg, Event, EventState(..), ProdState)
import Zenoss.Http.Devices as ZenossDevices
import Zenoss.Http.Events as ZenossEvents
import Zenoss.Html

-- function to handle RefreshDevices Msg
refreshDevices: Model -> Cmd Msg
refreshDevices model =
    let responseHandler = Main.Model.NewDevices
        request = ZenossDevices.get {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        }
    in Http.send responseHandler request


-- function to handle UpdateDevice Msg
updateDevice: ProdState -> String -> Model -> (Model, Cmd Msg)
updateDevice ps uid model =
    let responseHandler = Main.Model.UpdateDeviceResponse
        request = ZenossDevices.setInfo {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        } uid ps
    in
        (model, Cmd.none)

-- function to handle RefreshEvents Msg
refreshEvents: Model -> Cmd Msg
refreshEvents model =
    let responseHandler = Main.Model.NewEvents
        request = ZenossEvents.query {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        }
    in
        Http.send responseHandler request


-- function to handle AcknowledgeEvent Msg
acknowledgeEvent: Model -> String -> (Model, Cmd Msg)
acknowledgeEvent model eventId =
    let responseHandler = Main.Model.AcknowledgeResponse eventId
        request = ZenossEvents.acknowledge {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        } eventId
        events = changeEventState model eventId -- optimistic update
    in
        {model | events = events}
        ! [
            Navigation.newUrl "#Events",
            Http.send responseHandler request
        ]

unacknowledgeEvent:  Model -> String -> (Model, Cmd Msg)
unacknowledgeEvent model eventId =
    let responseHandler = Main.Model.UnacknowledgeResponse eventId
        request = ZenossEvents.unacknowledge {
            hostname = model.hostname,
            username = model.username,
            password = model.password
        } eventId
        events = changeEventState model eventId -- optimistic update
    in
        {model | events = events}
        ! [
            Navigation.newUrl "#Events",
            Http.send responseHandler request
        ]

changeEventState: Model -> String -> List Event
changeEventState model eventId =
    List.map (\event ->
        if event.id == eventId
            then
                case event.eventState of
                    New -> {event | eventState = Acknowledged}
                    Acknowledged -> {event | eventState = New}
            else event
    ) model.events


-- function to handle DevicesPage View
devicesView: Model -> Html Msg
devicesView model =
    div [] [
        Zenoss.Html.renderDeviceList model.devices
    ]


-- function to handleDevicePage View
deviceDetailView: Model -> String -> Html Msg
deviceDetailView model uid =
    div [] [
        Zenoss.Html.renderDeviceDetails model.devices uid
    ]

-- function to handle EventsPage View
eventsView: Model -> Html Msg
eventsView model =
    div [] [
        Zenoss.Html.renderEventList model.events
    ]


-- function to handle EventPage View
eventDetailView: Model -> String -> Html Msg
eventDetailView model eid =
    div [] [
        Zenoss.Html.renderEventDetails model.events eid
    ]


errorEvent: String -> Main.Model.Event
errorEvent err =
    Event "error"
          "zenoss-web"
          ("Failed to connect to Zenoss server: " ++ toString err)
          ("Production", 1000)
          "Critical"
          New Nothing
          1
          "null"
          "null"
          Nothing
          "N/A"