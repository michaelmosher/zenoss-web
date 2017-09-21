import Html exposing (div, h2, a, text)
import Html.Attributes exposing (href)
import Navigation
import UrlParser as Url

import Cats

main : Program Never Model Msg
main =
  Navigation.program UrlChange {
    init = (\l -> (Model (Url.parseHash route l), Cmd.none)),
    view =  view,
    update =  update,
    subscriptions = (\_ -> Sub.none)
  }

type Page = Home | Cats

type alias Model = {
  currentPage : Maybe Page
}

route : Url.Parser (Page -> a) a
route =
  Url.oneOf [
    Url.map Home Url.top,
    Url.map Cats (Url.s "cats")
  ]


type Msg =
  UrlChange Navigation.Location
  | GoHome

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange location ->
      ({model | currentPage = Url.parseHash route location}, Cmd.none)

    GoHome ->
      ({model | currentPage = (Just Home)}, Cmd.none)


view: Model -> Html.Html Msg
view m =
  case m.currentPage of
    Just Cats ->
      let
        callback = GoHome
      in
        Cats.view m callback

    _ ->
      div [] [
        h2 [] [text "hello world"],
        a [href "#cats"] [text "Cats"]
      ]
