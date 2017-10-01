module Zenoss.Http.Events exposing (query, acknowledge, unacknowledge)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optionalAt, required, requiredAt)
import Json.Encode as Json
import Main.Model exposing (Event, EventState(..), ProdState)
import Zenoss.Http.Shared exposing (Auth, apiRequest, apiRequestBody)


query: Auth -> Http.Request (List Event)
query auth =
    let body = queryData |> eventsRequestBody "query"
    in
        eventRequest auth body queryDecoder


acknowledge: Auth -> String -> Http.Request Bool
acknowledge auth eventId =
    let body = acknowledgeData eventId |> eventsRequestBody "acknowledge"
    in
        eventRequest auth body acknowledgeDecoder

unacknowledge: Auth -> String -> Http.Request Bool
unacknowledge auth eventId =
    let body = unacknowledgeData eventId |> eventsRequestBody "unacknowledge"
    in
        eventRequest auth body unacknowledgeDecoder


eventRequest: Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
eventRequest =
    apiRequest "evconsole_router"


eventsRequestBody: String -> Json.Value -> Http.Body
eventsRequestBody =
    apiRequestBody "EventsRouter"


queryData: Json.Value
queryData =
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


acknowledgeData: String -> Json.Value
acknowledgeData eventId =
    Json.list [
        Json.object [
            ("evids", Json.list [
                Json.string eventId
            ])
        ]
    ]

unacknowledgeData: String -> Json.Value
unacknowledgeData = acknowledgeData

queryDecoder: Decode.Decoder (List Event)
queryDecoder =
    Decode.field "result"
        (Decode.field "events"
            (Decode.list eventDecoder)
        )


acknowledgeDecoder: Decode.Decoder Bool
acknowledgeDecoder =
    Decode.field "result"
        (Decode.field "success" Decode.bool)


unacknowledgeDecoder: Decode.Decoder Bool
unacknowledgeDecoder = acknowledgeDecoder


eventDecoder: Decode.Decoder Event
eventDecoder =
    decode Event
        |> required "id" Decode.string
        |> requiredAt ["device", "text"] Decode.string
        |> required "summary" Decode.string
        |> required "prodState" prodStateDecoder
        |> required "severity"severityDecoder
        |> required "eventState" eventStateDecoder
        |> required "ownerid" (Decode.maybe Decode.string)
        |> required "count" Decode.int
        |> required "firstTime" Decode.string
        |> required "lastTime" Decode.string
        |> requiredAt ["component", "text"] (Decode.nullable Decode.string)
        |> optionalAt ["details", "stderr"] stderrDecoder "N/A"


prodStateDecoder: Decode.Decoder ProdState
prodStateDecoder =
    Decode.string |> Decode.andThen (\ps ->
            case ps of
                "Production" -> Decode.succeed ("Production", 1000)
                "Pre-Production" -> Decode.succeed ("Pre-Production", 500)
                "Test" -> Decode.succeed ("Test", 400)
                "Maintenance" -> Decode.succeed ("Maintenance", 300)
                "Decommissioned" -> Decode.succeed ("Decommissioned", -1)
                c -> Decode.succeed (c, 0)
        )


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
