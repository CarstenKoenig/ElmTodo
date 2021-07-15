module Models.Tasks exposing
    ( Filter(..)
    , Tasks
    , activeTasks
    , allCompleted
    , allTasks
    , completedTasks
    , deleteTask
    , empty
    , fromList
    , get
    , insertTask
    , isEmpty
    , setCompleted
    , setText
    , toggle
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Models.Task as Task exposing (Task)


type Filter
    = All
    | Active
    | Completed


type Tasks
    = Tasks
        { dict : Dict Int Task
        }


empty : Tasks
empty =
    Tasks { dict = Dict.empty }


isEmpty : Tasks -> Bool
isEmpty (Tasks tasks) =
    Dict.isEmpty tasks.dict


allCompleted : Tasks -> Bool
allCompleted tasks =
    List.length (allTasks tasks) == List.length (completedTasks tasks)


get : Task.Id -> Tasks -> Maybe Task
get tId (Tasks tasks) =
    Dict.get (Task.idToInt tId) tasks.dict


fromList : List Task -> Tasks
fromList tasks =
    let
        dict =
            tasks
                |> List.map (\task -> ( Task.idToInt task.id, task ))
                |> Dict.fromList
    in
    Tasks { dict = dict }


allTasks : Tasks -> List Task
allTasks =
    toList


activeTasks : Tasks -> List Task
activeTasks =
    List.filter (\task -> task.completed == False) << toList


completedTasks : Tasks -> List Task
completedTasks =
    List.filter (\task -> task.completed == True) << toList


toList : Tasks -> List Task
toList (Tasks tasks) =
    Dict.values tasks.dict


insertTask : Task -> Tasks -> Tasks
insertTask task (Tasks tasks) =
    Tasks { tasks | dict = Dict.insert (Task.idToInt task.id) task tasks.dict }


deleteTask : Task.Id -> Tasks -> Tasks
deleteTask id =
    updateTask (always Nothing) id
        >> Tuple.second


setCompleted : Bool -> Task.Id -> Tasks -> ( Maybe Task, Tasks )
setCompleted completed =
    updateTask (Maybe.map (\task -> { task | completed = completed }))


toggle : Task.Id -> Tasks -> ( Maybe Task, Tasks )
toggle =
    updateTask (Maybe.map (\task -> { task | completed = not task.completed }))


setText : String -> Task.Id -> Tasks -> ( Maybe Task, Tasks )
setText text =
    updateTask (Maybe.map (\task -> { task | text = text }))


updateTask : (Maybe Task -> Maybe Task) -> Task.Id -> Tasks -> ( Maybe Task, Tasks )
updateTask update tId (Tasks tasks) =
    let
        taskNr =
            Task.idToInt tId

        foundTask =
            Dict.get taskNr tasks.dict

        updatedTask =
            update foundTask

        newDict =
            case updatedTask of
                Nothing ->
                    Dict.remove taskNr tasks.dict

                Just task ->
                    Dict.insert taskNr task tasks.dict
    in
    ( updatedTask, Tasks { tasks | dict = newDict } )
