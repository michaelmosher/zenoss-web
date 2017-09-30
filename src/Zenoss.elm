module Zenoss exposing (..)

import Html exposing (Html, div, h2, text)
import Http
import Navigation

import Main.Model exposing (Model, Msg, Event, EventState(..))
import Zenoss.Http
import Zenoss.Html

-- function to handle FetchEvents Msg
refreshEvents: Model -> Cmd Msg
refreshEvents model =
    let responseHandler = Main.Model.NewEvents
        request = Zenoss.Http.queryEvents {
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
        request = Zenoss.Http.acknowledgeEvents {
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
        request = Zenoss.Http.unacknowledgeEvents {
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
          "Production"
          "Critical"
          New Nothing
          1
          "null"
          "null"
          Nothing
          "N/A"