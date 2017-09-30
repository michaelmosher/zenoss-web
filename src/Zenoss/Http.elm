module Zenoss.Http exposing (queryEvents, acknowledgeEvents, unacknowledgeEvents)

import Base64
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optionalAt, required, requiredAt)
import Json.Encode as Json

import Main.Model exposing (Device, Event, EventState(..))

type alias Auth = {
    hostname: String,
    username: String,
    password: String
}

queryEvents: Auth -> Http.Request (List Event)
queryEvents auth =
    let body = eventsRequestBody "query" queryEventData
    in
        eventRequest auth body queryEventsDecoder


acknowledgeEvents: Auth -> String -> Http.Request Bool
acknowledgeEvents auth eventId =
    let body = acknowledgeEventsData eventId |> eventsRequestBody "acknowledge"
    in
        eventRequest auth body acknowledgeEventsDecoder

unacknowledgeEvents: Auth -> String -> Http.Request Bool
unacknowledgeEvents auth eventId =
    let body = acknowledgeEventsData eventId |> eventsRequestBody "unacknowledge"
    in
        eventRequest auth body acknowledgeEventsDecoder


getDevices: Auth -> Http.Request (List Device)
getDevices auth =
    let body = deviceRequestBody "getDevice" getDeviceData
    in
        deviceRequest auth body getDevicesDecoder


eventRequest: Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
eventRequest =
    apiRequest "evconsole_router"


deviceRequest: Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
deviceRequest =
    apiRequest "device_router"


apiRequest: String -> Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
apiRequest router auth body decoder =
    let username = auth.username
        password = auth.password
        authString = "Basic " ++ Base64.encode(username ++ ":" ++ password)
    in Http.request {
        method = "POST",
        headers = [
            Http.header "Authorization" authString
        ],
        url = "https://" ++ auth.hostname ++ "/zport/dmd/" ++ router,
        body = body,
        expect = Http.expectJson decoder,
        timeout = Nothing,
        withCredentials = False
    }


queryEventData: Json.Value
queryEventData =
    let filter = Json.object [
            ("eventState", Json.list [
                Json.int 0,
                Json.int 1
            ]),
            ("prodState", Json.list [
                Json.int 1000,
                Json.int 500,
                Json.int 400,
                Json.int 300
            ])
        ]
    in
        Json.list [
            Json.object [
                ("limit", Json.int 20),
                ("sort", Json.string "severity"),
                ("params", filter)
            ]
        ]


acknowledgeEventsData: String -> Json.Value
acknowledgeEventsData eventId =
    Json.list [
        Json.object [
            ("evids", Json.list [
                Json.string eventId
            ])
        ]
    ]


getDeviceData: Json.Value
getDeviceData =
    Json.list [
        Json.object [
            ("keys", Json.list [
                    Json.string "uid",
                    Json.string "name",
                    Json.string "prodState",
                    Json.string "ipAddressString"
            ]),
            ("uid", Json.string "/zport/dmd/Server")
        ]
    ]


eventsRequestBody: String -> Json.Value -> Http.Body
eventsRequestBody =
    apiRequestBody "EventsRouter"


deviceRequestBody: String -> Json.Value -> Http.Body
deviceRequestBody = 
    apiRequestBody "DeviceRouter"


apiRequestBody: String -> String -> Json.Value -> Http.Body
apiRequestBody action method data =
    Json.object [
        ("action", Json.string action),
        ("method", Json.string method),
        ("tid", Json.int 1),
        ("data", data)
      ] |> Http.jsonBody


queryEventsDecoder: Decode.Decoder (List Event)
queryEventsDecoder =
    Decode.field "result"
        (Decode.field "events"
            (Decode.list eventDecoder)
        )


acknowledgeEventsDecoder: Decode.Decoder Bool
acknowledgeEventsDecoder =
    Decode.field "result"
        (Decode.field "success" Decode.bool)

eventDecoder: Decode.Decoder Event
eventDecoder =
    decode Event
        |> required "id" Decode.string
        |> requiredAt ["device", "text"] Decode.string
        |> required "summary" Decode.string
        |> required "prodState" Decode.string
        |> required "severity"severityDecoder
        |> required "eventState" eventStateDecoder
        |> required "ownerid" (Decode.maybe Decode.string)
        |> required "count" Decode.int
        |> required "firstTime" Decode.string
        |> required "lastTime" Decode.string
        |> requiredAt ["component", "text"] (Decode.nullable Decode.string)
        |> optionalAt ["details", "stderr"] stderrDecoder "N/A"


severityDecoder: Decode.Decoder String
severityDecoder =
    Decode.int |> Decode.andThen (\num ->
        case num of
            5 -> Decode.succeed "Critical"
            4 -> Decode.succeed "Error"
            3 -> Decode.succeed "Warning"
            2 -> Decode.succeed "Info"
            u -> Decode.fail <| "Unknown Severity: " ++ (toString u)
    )


eventStateDecoder: Decode.Decoder EventState
eventStateDecoder =
    Decode.string |> Decode.andThen (\str ->
        case str of
            "New" -> Decode.succeed New
            "Acknowledged" -> Decode.succeed Acknowledged
            somethingElse -> Decode.fail <| "Uknown EventState: " ++ somethingElse
    )


stderrDecoder: Decode.Decoder String
stderrDecoder =
    Decode.list Decode.string |> Decode.andThen (\l ->
        Decode.succeed (List.foldr (++) "" l)
    )


getDevicesDecoder: Decode.Decoder (List Device)
getDevicesDecoder =
    Decode.field "result"
        (Decode.field "devices"
            (Decode.list deviceDecoder)
        )

deviceDecoder: Decode.Decoder Device
deviceDecoder =
    decode Device
        |> required "uid" Decode.string
        |> required "name" Decode.string
        |> required "prodState" deviceProdStateDecoder
        |> required "ipAddressString" Decode.string


deviceProdStateDecoder: Decode.Decoder String
deviceProdStateDecoder =
    Decode.int |> Decode.andThen (\int ->
            case int of
                1000 -> Decode.succeed "Production"
                500 -> Decode.succeed "Pre-Production"
                400 -> Decode.succeed "Test"
                300 -> Decode.succeed "Maintenance"
                (-1) -> Decode.succeed "Decommissioned"
                c -> Decode.succeed <| "Custom: " ++ (toString c)
        )