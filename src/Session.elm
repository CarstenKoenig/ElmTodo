module Session exposing
    ( Session
    , init
    )

import Flags exposing (Flags)


type alias Session =
    { flags : Flags
    }


init : Flags -> Session
init flags =
    Session flags
