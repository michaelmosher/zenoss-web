module Cats exposing (view)

import Html exposing (div, h2, button, text)
import Html.Events exposing (onClick)


view: a -> b -> Html.Html b
view _ callback =
  div [] [
    h2 [] [text "Cats!"],
    button [onClick callback] [text "Go Home"]
  ]
