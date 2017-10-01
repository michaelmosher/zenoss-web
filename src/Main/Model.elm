module Main.Model exposing (..)

import Http
import Navigation

type alias Setting = {
    key:  String,
    value: Maybe String
}

type Page = LoginPage | DevicesPage | EventsPage | EventPage String

type EventState = New | Acknowledged

type alias Event = {
    id: String,
    deviceName: String,
    summary: String,
    prodState: String,
    severity: String,
    eventState: EventState,
    owner: Maybe String,
    count: Int,
    firstTime: String,
    lastTime: String,
    component: Maybe String,
    stdErr: String
}

type alias Device = {
    uid: String,
    name: String,
    deviceClass: String,
    prodState: String,
    ipAddress: String,
    critEvents: Int,
    errEvents: Int,
    warnEvents: Int
}

type alias Model = {
    currentPage : Maybe Page,
    hostname: String,
    username: String,
    password: String,
    devices: List Device,
    events: List Event
}

type Msg =
    UrlChange Navigation.Location
    | NewSetting Setting
    | UpdateHostname String
    | UpdateUsername String
    | UpdatePassword String
    | LoginMsg
    | RefreshEvents
    | NewEvents (Result Http.Error (List Event))
    | EventDetails String
    | AcknowledgeEvent String
    | AcknowledgeResponse String (Result Http.Error (Bool))
    | UnacknowledgeEvent String
    | UnacknowledgeResponse String (Result Http.Error (Bool))
    | RefreshDevices
    | NewDevices (Result Http.Error (List Device))
