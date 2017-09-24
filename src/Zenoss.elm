module Zenoss exposing (eventsRequest)

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

eventsRequest: Auth -> Http.Request (List Event)
eventsRequest auth =
  let
    username = auth.username
    password = auth.password
    authString = "Basic " ++ Base64.encode(username ++ ":" ++ password)
  in Http.request {
    method = "POST",
    headers = [
      Http.header "Authorization" authString
    ],
    url = "https://" ++ auth.hostname ++ "/zport/dmd/evconsole_router",
    body = eventsRequestBody,
    expect = Http.expectJson decodeEventsResponse,
    timeout = Nothing,
    withCredentials = False
  }

eventsRequestBody: Http.Body
eventsRequestBody =
  Json.object [
      ("action", Json.string "EventsRouter"),
      ("method", Json.string "query"),
      ("tid", Json.int 1),
      ("data", Json.list [
        Json.object [
          ("limit", Json.int 20),
          ("sort", Json.string "severity"),
          ("params", Json.object [
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
          ])
        ]
      ])
    ] |> Http.jsonBody


decodeEventsResponse: Decode.Decoder (List Event)
decodeEventsResponse =
  Decode.field "result"
    (Decode.field "events"
      (Decode.list eventDecoder)
    )


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

