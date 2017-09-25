port module LocalSettings exposing(..)

import Main.Model exposing (Model, Setting)

-- port for saving LocalSettings
port setSetting: (Setting) -> Cmd msg

-- port for LocalSetting `gets` requests
port getSetting: String -> Cmd msg

-- port for LocalSetting `get` responses
port newSetting: (Setting -> msg) -> Sub msg

loadInitialSettings: Cmd msg
loadInitialSettings =
    Cmd.batch [
        getSetting "Zhostname",
        getSetting "Zusername",
        getSetting "Zpassword"
    ]

storeSetting: Setting -> Model -> Model
storeSetting s model =
    let value = case s.value of
            Just s -> s
            Nothing -> ""
    in
        case s.key of
            "Zhostname" -> {model | hostname = value}
            "Zusername" -> {model | username = value}
            "Zpassword" -> {model | password = value}
            _ -> model