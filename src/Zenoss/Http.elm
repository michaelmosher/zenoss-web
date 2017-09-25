module Zenoss.Http exposing (queryEvents)

import Base64
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optionalAt, required, requiredAt)
import Json.Encode as Json

import Main.Model exposing (Event, EventState(..))

type alias Auth = {
    hostname: String,
    username: String,
    password: String
}

queryEvents: Auth -> Http.Request (List Event)
queryEvents auth =
    let body = eventsRequestBody "query" queryEventData
    in
        eventsRequest auth body queryEventsDecoder


acknowledgeEvents: Auth -> List String -> Http.Request Bool
acknowledgeEvents auth eventIds =
    let body = acknowledgeEventsData eventIds |> eventsRequestBody "acknowledge"
    in
        eventsRequest auth body acknowledgeEventsDecoder -- <- TODO

eventsRequest: Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
eventsRequest auth body decoder =
    let username = auth.username
        password = auth.password
        authString = "Basic " ++ Base64.encode(username ++ ":" ++ password)
    in Http.request {
        method = "POST",
        headers = [
            Http.header "Authorization" authString
        ],
        url = "https://" ++ auth.hostname ++ "/zport/dmd/evconsole_router",
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


acknowledgeEventsData: List String -> Json.Value
acknowledgeEventsData eventIds =
    Json.list [
        Json.object [
            ("evids", List.map Json.string eventIds |> Json.list)
        ]
    ]


eventsRequestBody: String -> Json.Value -> Http.Body
eventsRequestBody method data =
    Json.object [
        ("action", Json.string "EventsRouter"),
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

