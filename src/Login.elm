module Login exposing (pageView)

import Html exposing (h1, div, p, label, input, button, text)
import Html.Attributes exposing (autofocus, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)

import Main.Model exposing (Msg(..), Model)

pageView: Model -> Html.Html Msg
pageView model =
    div [style [("text-align", "center"), ("width", "100%")]] [
        h1 [] [text "Login"],
        div [style [("width", "75%"), ("display", "flex"), ("flex-direction", "column"), ("margin", "auto")]] [
            hostnameLoginField model.hostname,
            usernameLoginField model.username,
            passwordLoginField model.password,
            button [style [("font-size", "x-large")], onClick LoginMsg] [text "Fetch Events"]
        ]
    ]


hostnameLoginField: String -> Html.Html Msg
hostnameLoginField hostname =
    let defaultValue = if hostname == ""
        then (placeholder "Zenoss Hostname")
        else (value hostname)
    in  
        loginField [defaultValue] UpdateHostname


usernameLoginField: String -> Html.Html Msg
usernameLoginField username =
    let defaultValue = if username == ""
        then (placeholder "Zenoss Username")
        else (value username)
    in  
        loginField [defaultValue] UpdateUsername


passwordLoginField: String -> Html.Html Msg
passwordLoginField password =
    let defaultValue = if password == ""
        then (placeholder "Zenoss Password")
        else (value password)
    in  
        loginField [defaultValue, (type_ "password")] UpdatePassword


loginField: List (Html.Attribute Msg) -> (String -> Msg) -> Html.Html Msg
loginField attrs inputMsg =
    let css = style [
            ("font-size", "x-large"),
            ("margin-bottom", "5px"),
            ("padding-left", "3px")
        ]
        inputAttrs = attrs ++ [css, (onInput inputMsg)]
    in
        input inputAttrs []

