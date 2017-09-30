module Main.Html exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Main.Model exposing (Msg(..), Page(..))

overlay: List (Html Msg) -> Maybe Page -> Html Msg
overlay children currentPage =
    let css = style[
            ("width", "100%"),
            ("height", "100%")
        ]
    in
        div [css] [
            header,
            div [style[("padding", "10px 0"), ("margin-bottom", "20px")]] children,
            footer currentPage
        ]

header: Html Msg
header =
    let css = style [
            ("display", "flex"),
            ("color", "white")
        ]
    in
        div [css] [
            dashboardButton,
            text "|",
            eventsButton,
            text "|",
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
    div [sharedButtonStyle] [text "Dashboard"]


eventsButton: Html Msg
eventsButton =
    div [sharedButtonStyle, onClick RefreshEvents] [text "Events"]


devicesButton: Html Msg
devicesButton =
    div [sharedButtonStyle] [text "Devices"]


refreshButton: Maybe Page -> Html Msg
refreshButton page =
    let refreshAction = case page of
            Just EventsPage -> Just RefreshEvents
            Just (EventPage _) -> Just RefreshEvents
            _ -> Nothing
        buttonAttributes = case refreshAction of
            Just msg -> [Html.Events.onClick msg] ++ [sharedButtonStyle]
            Nothing -> [sharedButtonStyle]

    in
        div buttonAttributes [text "Refresh"]


sharedButtonStyle : Html.Attribute a
sharedButtonStyle =
    style [
        ("flex", "1 1 0"),
        ("background-color", "#001f3f"),
        ("color", "white"),
        ("height", "30px"),
        ("line-height", "30px"),
        ("text-align", "center"),
        ("font-weight", "bold"),
        ("font-size", "1.2em")
    ]
