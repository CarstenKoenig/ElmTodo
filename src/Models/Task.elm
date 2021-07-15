module Models.Task exposing (Id, Task, idFromInt, idToInt, idToString)


type alias Task =
    { id : Id
    , text : String
    , completed : Bool
    }


type Id
    = Id Int


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idToInt : Id -> Int
idToInt (Id id) =
    id


idFromInt : Int -> Id
idFromInt =
    Id
