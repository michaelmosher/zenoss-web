module Zenoss.Html exposing (renderEventList, renderEventDetails)

import Char
import Html exposing (Html, div, span, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes exposing (points, fill, cx, cy, r)
import Main.Model exposing (Event, EventState(..), Msg(..))

renderEventList: List Event -> Html Msg
renderEventList events =
    let listCss = style [
            ("border", "solid 5px black"),
            ("border-radius", "20px")
        ]
    in
        div [listCss] (List.map renderSimpleEvent events)


renderEventDetails: List Event -> String -> Html Msg
renderEventDetails events eid =
    let event = List.filter (\e -> e.id == eid) events |> List.head
    in
        case event of
            Nothing -> p [] [text "Error!"]
            Just e -> renderDetailedEvent e


renderSimpleEvent: Event -> Html Msg
renderSimpleEvent event =
    let bgColor = case event.eventState of
            New -> "white"
            Acknowledged -> "lightgrey"
        mainCss = style [
            ("position", "relative"),
            ("display", "flex"),
            ("border-bottom", "solid 1px black"),
            ("margin", "0"),
            ("padding", "5px"),
            ("align-items", "center"),
            ("background-color", bgColor)
        ]
    in
        div [class "simple-event", mainCss, (onClick (EventDetails event.id))] [
            event.severity |> renderEventSeverity,
            div [style [("flex-grow", "1")]] [
                div [style [("display", "flex")]] [
                    event.deviceName |> renderEventDeviceName,
                    event.prodState |> renderEventProdState
                ],
                event.summary |> renderEventSummary,
                div [style [("display", "flex")]] [
                    event.count |> renderEventCount,
                    event.owner |> renderEventOwner
                ]
            ],
            event.eventState |> renderEventState
        ]


renderDetailedEvent: Event -> Html Msg
renderDetailedEvent event =
    let component = case event.component of
            Just c -> c
            Nothing -> "null"
        ownerString = case event.owner of
            Nothing -> "Not ack'd"
            Just o -> "Ack'd by " ++ o
    in
        div [class "detailed-event"] [
            eventDetailField "device" event.deviceName,
            eventDetailField "component" component,
            eventDetailField "summary" event.summary,
            eventDetailField "prod state" event.prodState,
            eventDetailField "severity" event.severity,
            eventDetailField "owner" ownerString,
            eventDetailField "first occurance" event.firstTime,
            eventDetailField "latest occurance" event.lastTime,
            eventDetailField "error output" event.stdErr
        ]
        

renderEventSeverity: String -> Html a
renderEventSeverity s =
    let svgAttrs = [
            style [
                ("display", "block"),
                ("margin", "auto")
            ],
            Svg.Attributes.width "30",
            Svg.Attributes.height "30"
        ]
    in
        div [style [("width", "50px")]] [
            Svg.svg svgAttrs [severityShape s]
        ]


renderEventDeviceName: String -> Html a
renderEventDeviceName d =
    let css = style [
            ("font-weight", "bolder"),
            ("flex-grow", "1")
        ]
    in
        span [css] [text d]


renderEventProdState: String -> Html a
renderEventProdState p =
    span [] [text p]


renderEventSummary: String -> Html a
renderEventSummary s =
    p [] [text s]


renderEventCount: Int -> Html a
renderEventCount c =
    let css = style [
            ("flex-grow", "1")
        ]
    in
        span [css] ["Count: " ++ toString c |> text]


renderEventOwner: Maybe String -> Html a
renderEventOwner owner =
    let ownerString = case owner of
            Nothing -> "Not ack'd"
            Just o -> "Ack'd by " ++ o
    in
        span [] [text ownerString]


renderEventState: EventState -> Html a
renderEventState s =
    let staticStyles = [
            ("width", "50px"),
            ("text-align", "center")
        ]
        dynamicStyles = case s of
            New -> [("font-size", "3em"), ("color", "inherit")]
            Acknowledged -> [("font-size", "1.5em"), ("color", "green")]
        css = staticStyles ++ dynamicStyles |> style
    in
        div [css] [s |> eventStateString |> text]


eventStateString: EventState -> String
eventStateString e =
    case e of
        New -> String.fromChar (Char.fromCode 9675) -- ○
        Acknowledged -> String.fromChar (Char.fromCode 10003) -- ✓


severityShape: String -> Svg.Svg a
severityShape s =
    case s of
        "Critical" -> Svg.polygon [fill "red", points "11,5 19,5 25,11 25,19 19,25 11,25 5,19 5,11"] []
        "Error" -> Svg.polygon [fill "orange", points "5,5 15,25 25,5"] []
        "Warning" -> Svg.polygon [fill "yellow", points "15,5 5,25 25,25"] []
        _ -> Svg.circle [fill "blue", cx "15", cy "15", r "10"] []


eventDetailField: String -> String -> Html a
eventDetailField key value =
    div [style [("display", "flex")]] [
        span [style [("border-bottom", "lightgrey solid 1px"), ("font-weight", "bolder"), ("flex-grow", "1")]] [text key],
        span [style [("border-bottom", "lightgrey solid 1px")]] [text value]
    ]

