module Main.Html exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Main.Model exposing (Msg(..), Page(..))

overlay: List (Html Msg) -> Maybe Page -> Html Msg
overlay children currentPage =
    let parentCss = style [
            ("width", "100%"),
            ("height", "100%")
        ]
        childrenCss = style [
            ("padding", "10px, 0"),
             ("margin-top", "45px"),
             ("margin-bottom", "45px")
        ]
    in
        div [parentCss] [
            header,
            div [childrenCss] children,
            footer currentPage
        ]

header: Html Msg
header =
    let css = style [
            ("display", "flex"),
            ("position", "fixed"),
            ("top", "0"),
            ("width", "100%"),
            ("color", "white"),
            ("z-index", "2"),
            ("background-color", "white")
        ]
    in
        div [css] [
            dashboardButton,
            eventsButton,
            devicesButton
        ]


footer: Maybe Page -> Html Msg
footer page =
    let css = style [
            ("position", "fixed"),
            ("bottom", "0"),
            ("width", "100%")
        ]
    in
        div [css] [
            refreshButton page
        ]


dashboardButton: Html Msg
dashboardButton =
    div [style sharedButtonStyle, onClick ShowDashboard] [text "Dashboard"]


eventsButton: Html Msg
eventsButton =
    let css = [
            ("border-left", "white solid 3px"),
            ("border-right", "white solid 3px")
        ] ++ sharedButtonStyle
    in
        div [style css, onClick RefreshEvents] [text "Events"]


devicesButton: Html Msg
devicesButton =
    div [style sharedButtonStyle, onClick RefreshDevices] [text "Devices"]


refreshButton: Maybe Page -> Html Msg
refreshButton page =
    let refreshAction = case page of
            Just DevicesPage -> Just RefreshDevices
            Just EventsPage -> Just RefreshEvents
            Just (EventPage _) -> Just RefreshEvents
            _ -> Nothing
        buttonAttributes = case refreshAction of
            Just msg -> [Html.Events.onClick msg] ++ [style sharedButtonStyle]
            Nothing -> [style sharedButtonStyle]

    in
        div buttonAttributes [text "Refresh"]


sharedButtonStyle : List ( String, String)
sharedButtonStyle =
    [
        ("flex", "1 1 0"),
        ("background-color", "#001f3f"),
        ("color", "white"),
        ("height", "45px"),
        ("line-height", "45px"),
        ("text-align", "center"),
        ("font-weight", "bold"),
        ("font-size", "1.2em")
    ]
