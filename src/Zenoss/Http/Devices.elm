module Zenoss.Http.Devices exposing (get)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optional, required, requiredAt)
import Json.Encode as Json
import Main.Model exposing (Device)
import Zenoss.Http.Shared exposing (Auth, apiRequest, apiRequestBody)

get: Auth -> Http.Request (List Device)
get auth =
    let body = deviceRequestBody "getDevices" getData
    in
        deviceRequest auth body getDecoder


deviceRequest: Auth -> Http.Body -> Decode.Decoder a -> Http.Request a
deviceRequest =
    apiRequest "device_router"


deviceRequestBody: String -> Json.Value -> Http.Body
deviceRequestBody = 
    apiRequestBody "DeviceRouter"


getData: Json.Value
getData =
    Json.list [
        Json.object [
            ("keys", Json.list [
                    Json.string "uid",
                    Json.string "name",
                    Json.string "deviceClass",
                    Json.string "productionState",
                    Json.string "ipAddressString",
                    Json.string "events"
            ]),
            ("limit", Json.int 300),
            ("uid", Json.string "/zport/dmd/Devices/Server")
        ]
    ]


getDecoder: Decode.Decoder (List Device)
getDecoder =
    Decode.field "result"
        (Decode.field "devices"
            (Decode.list deviceDecoder)
        )

deviceDecoder: Decode.Decoder Device
deviceDecoder =
    decode Device
        |> required "uid" Decode.string
        |> required "name" Decode.string
        |> requiredAt ["deviceClass", "name"] Decode.string
        |> required "productionState" deviceProdStateDecoder
        |> optional "ipAddressString" Decode.string "unknown"
        |> requiredAt ["events", "critical", "count"] Decode.int
        |> requiredAt ["events", "error", "count"] Decode.int
        |> requiredAt ["events", "warning", "count"] Decode.int


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
