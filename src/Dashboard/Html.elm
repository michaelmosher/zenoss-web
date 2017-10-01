module Dashboard.Html exposing (renderDashboard)

import Html exposing (Html, h1, article, p, ul, li, a, text)
import Html.Attributes exposing (href)

renderDashboard: Html a
renderDashboard =
    article [] [
        h1 [] [text "Welcome!"],
        p [] [text "This is a mobile-friendly interface for Zenoss. Current features include:"],
        ul [] [
            li [] [text "view current event list"],
            li [] [text "view event details"],
            li [] [text "acknowledge and unacknowledge events"],
            li [] [text "view device summaries"]
        ],
        p [] [text "Upcoming features include:"],
        ul [] [
            li [] [text "close events (if anyone requests it)"],
            li [] [text "easily modify device production state"],
            li [] [text "view device details (please submit feedback on which details should be included)"],
            li [] [text "custom event filters"],
            li [] [text "view and add event log message (if anyone requests it)"]
        ],
        p [] [text "To submit feedback, please create an issue on ", a [href "https://github.com/michaelmosher/zenoss-web"] [text "GitHub."]],
        p [] [text "This is beta version 1."]
    ]
