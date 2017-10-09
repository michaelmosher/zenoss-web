module Zenoss.Http.Devices exposing (get, setInfo)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required, requiredAt)
import Json.Encode as Json
import Main.Model exposing (Device, ProdState)
import Zenoss.Http.Shared exposing (Auth, apiRequest, apiRequestBody)

get: Auth -> Http.Request (List Device)
get auth =
    let body = deviceRequestBody "getDevices" getData
    in
        deviceRequest auth body getDecoder


setInfo: Auth -> String -> ProdState -> Http.Request Bool
setInfo auth uid ps =
    let body = setInfoData uid ps |> deviceRequestBody "setInfo"
    in
        deviceRequest auth body setInfoDecoder


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


setInfoData: String -> ProdState -> Json.Value
setInfoData uid ps =
    let (_, numericValue) = ps
    in 
        Json.list [
            Json.object [
                ("uid", Json.string uid),
                ("productionState", Json.int numericValue)
            ]
        ]

getDecoder: Decode.Decoder (List Device)
getDecoder =
    Decode.field "result"
        (Decode.field "devices"
            (Decode.list deviceDecoder)
        )


setInfoDecoder: Decode.Decoder Bool
setInfoDecoder =
    Decode.field "result"
        (Decode.field "success" Decode.bool)


deviceDecoder: Decode.Decoder Device
deviceDecoder =
    decode Device
        |> required "uid" Decode.string
        |> required "name" Decode.string
        |> requiredAt ["deviceClass", "name"] Decode.string
        |> required "productionState" prodStateDecoder
        |> optional "ipAddressString" Decode.string "unknown"
        |> requiredAt ["events", "critical", "count"] Decode.int
        |> requiredAt ["events", "error", "count"] Decode.int
        |> requiredAt ["events", "warning", "count"] Decode.int
        |> hardcoded Nothing

prodStateDecoder: Decode.Decoder ProdState
prodStateDecoder =
    Decode.int |> Decode.andThen (\int ->
            case int of
                1000 -> Decode.succeed ("Production", 1000)
                500 -> Decode.succeed ("Pre-Production", 500)
                400 -> Decode.succeed ("Test", 400)
                300 -> Decode.succeed ("Maintenance", 300)
                (-1) -> Decode.succeed ("Decommissioned", -1)
                c -> Decode.succeed ("Custom", c)
        )
