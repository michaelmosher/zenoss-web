module Main.Model exposing (..)

import Http
import Navigation

type alias Setting = {
    key:  String,
    value: Maybe String
}

type Page = LoginPage | Events

type EventState = New | Acknowledged

type alias Event = {
  id: String,
  deviceName: String,
  summary: String,
  prodState: String,
  severity: String,
  eventState: EventState,
  owner: Maybe String,
  count: Int
}

type alias Model = {
  currentPage : Maybe Page,
  hostname: String,
  username: String,
  password: String,
  events: List Event
}

type Msg =
  UrlChange Navigation.Location
  | NewSetting Setting
  | UpdateHostname String
  | UpdateUsername String
  | UpdatePassword String
  | LoginMsg
  | FetchEvents
  | NewEvents (Result Http.Error (List Event))