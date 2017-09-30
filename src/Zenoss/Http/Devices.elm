module Zenoss.Http.Devices exposing (get)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optionalAt, required, requiredAt)
import Json.Encode as Json
import Main.Model exposing (Device)
import Zenoss.Http.Shared exposing (Auth, apiRequest, apiRequestBody)

get: Auth -> Http.Request (List Device)
get auth =
    let body = deviceRequestBody "getDevice" getData
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
                    Json.string "prodState",
                    Json.string "ipAddressString"
            ]),
            ("uid", Json.string "/zport/dmd/Server")
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
