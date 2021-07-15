port module LocalStorage exposing (StorageKey, receive, request, store)

import Json.Decode exposing (Value)


type alias StorageKey =
    String


port store : ( StorageKey, Maybe Value ) -> Cmd msg


port request : StorageKey -> Cmd msg


port receive : (( StorageKey, Maybe String ) -> msg) -> Sub msg
