module Zenoss.Http.Shared exposing (Auth, apiRequest, apiRequestBody)

import Base64
import Json.Decode as Decode
import Json.Encode as Json
import Http

type alias Auth = {
    hostname: String,
    username: String,
    password: String
}

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


apiRequestBody: String -> String -> Json.Value -> Http.Body
apiRequestBody action method data =
    Json.object [
        ("action", Json.string action),
        ("method", Json.string method),
        ("tid", Json.int 1),
        ("data", data)
      ] |> Http.jsonBody