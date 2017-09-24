module Login exposing (pageView)

import Html exposing (h1, div, p, input, button, text)
import Html.Attributes exposing (autofocus, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)

import Main.Model exposing (Msg(..), Model)

pageView: Model -> Html.Html Msg
pageView model =
  div [style [("text-align", "center"), ("width", "100%")]] [
    h1 [] [text "Login"],
    div [style [("width", "75%"), ("display", "flex"), ("flex-direction", "column"), ("margin", "auto")]] [
        loginField "Zenoss Hostname" UpdateHostname [(autofocus True), (value model.hostname)],
        loginField "Zenoss Username" UpdateUsername [(value model.username)],
        loginField "Zenoss Password" UpdatePassword [(type_ "password"), (value model.password)],
        button [style [("font-size", "x-large")], onClick LoginMsg] [text "Fetch Events"]
    ]
  ]


loginField: String -> (String -> Msg) -> List (Html.Attribute Msg) -> Html.Html Msg
loginField label inputMsg attrs =
  let
      css = style [
          ("font-size", "x-large"),
          ("margin-bottom", "5px"),
          ("padding-left", "3px")
      ]
      inputAttrs = attrs ++ [css, (placeholder label), (onInput inputMsg)]
  in
    input inputAttrs []

