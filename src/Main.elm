module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Flags exposing (Flags)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Json
import Models.Task as Task exposing (Task)
import Models.Tasks as Tasks exposing (Filter(..), Tasks)
import Session exposing (Session)
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { session : Session
    , tasks : Tasks
    , newText : String
    , editingTask :
        Maybe
            { id : Task.Id
            , text : String
            }
    , activeFilter : Filter
    }


type Msg
    = NoOp
    | UpdateNewText String
    | ClearNewText
    | AddTask
    | CheckTask Task.Id Bool
    | DeleteTask Task.Id
    | EditTask Task
    | UpdateEditText String
    | FinishEdit
    | CancelEdit
    | ClearCompleted
    | ToggleAll Bool


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { session = Session.init flags
      , tasks = Tasks.empty
      , newText = ""
      , editingTask = Nothing
      , activeFilter = All
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


activeCount : Tasks -> Int
activeCount tasks =
    List.length (tasks |> Tasks.activeTasks)


filtered : Filter -> Tasks -> List Task
filtered filter =
    case filter of
        All ->
            Tasks.allTasks

        Active ->
            Tasks.activeTasks

        Completed ->
            Tasks.completedTasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNewText updatedText ->
            ( { model | newText = updatedText }, Cmd.none )

        ClearNewText ->
            ( { model | newText = "" }, Cmd.none )

        AddTask ->
            ( { model | newText = "" }, Cmd.none )

        CheckTask taskId completed ->
            let
                ( _, newTasks ) =
                    Tasks.setCompleted completed taskId model.tasks
            in
            ( { model | tasks = newTasks }, Cmd.none )

        DeleteTask taskId ->
            let
                newTasks =
                    Tasks.deleteTask taskId model.tasks
            in
            ( { model | tasks = newTasks }, Cmd.none )

        EditTask task ->
            let
                edit =
                    { id = task.id
                    , text = task.text
                    }
            in
            ( { model | editingTask = Just edit }
            , Task.attempt (always NoOp) (Dom.focus ("edit_" ++ Task.idToString task.id))
            )

        UpdateEditText updatedText ->
            let
                edit =
                    model.editingTask
                        |> Maybe.map (\task -> { task | text = updatedText })
            in
            ( { model | editingTask = edit }, Cmd.none )

        CancelEdit ->
            ( { model | editingTask = Nothing }, Cmd.none )

        FinishEdit ->
            let
                ( _, newTasks ) =
                    model.editingTask
                        |> Maybe.map
                            (\editingTask ->
                                Tasks.setText editingTask.text editingTask.id model.tasks
                            )
                        |> Maybe.withDefault ( Nothing, model.tasks )
            in
            ( { model
                | tasks = newTasks
                , editingTask = Nothing
              }
            , Cmd.none
            )

        ClearCompleted ->
            let
                delete tasks id =
                    Tasks.deleteTask id tasks

                ( deleteCmds, deletedTasks ) =
                    Tasks.completedTasks model.tasks
                        |> List.foldl
                            (\task ( cmds, _ ) ->
                                let
                                    newTasks =
                                        Tasks.deleteTask task.id model.tasks
                                in
                                ( cmds, newTasks )
                            )
                            ( [], model.tasks )
            in
            ( { model | tasks = deletedTasks }, Cmd.batch deleteCmds )

        ToggleAll setCompleted ->
            let
                toggle tasks id =
                    Tasks.setCompleted setCompleted id tasks

                ( toggleCmds, toggledTasks ) =
                    filtered model.activeFilter model.tasks
                        |> List.foldl
                            (\task ( cmds, tasks ) ->
                                let
                                    ( _, newTasks ) =
                                        toggle tasks task.id
                                in
                                ( Cmd.none :: cmds, newTasks )
                            )
                            ( [], model.tasks )
            in
            ( { model | tasks = toggledTasks }, Cmd.batch toggleCmds )


view : Model -> Document Msg
view model =
    { title = "Elm Todo"
    , body =
        [ H.section
            [ Attr.class "todoapp" ]
            [ viewHeader model
            , viewMain model
            , viewFooter model
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text "TODO" ]
        , H.input
            [ Attr.class "new-todo"
            , Attr.placeholder "what needs to be done?"
            , Attr.autofocus True
            , Attr.value model.newText
            , Ev.onInput UpdateNewText
            , onFinish AddTask ClearNewText
            , Ev.onBlur ClearNewText
            ]
            []
        ]


{-| should be hidden by default and shown when there are tasks
-}
viewMain : Model -> Html Msg
viewMain model =
    if Tasks.isEmpty model.tasks then
        H.text ""

    else
        H.section
            [ Attr.class "main" ]
            [ H.input
                [ Attr.id "toggle-all"
                , Attr.class "toggle-all"
                , Attr.type_ "checkbox"
                , Attr.checked (Tasks.allCompleted model.tasks)
                , Ev.onCheck ToggleAll
                ]
                []
            , H.label
                [ Attr.for "toggle-all" ]
                [ H.text "Mark all as complete" ]

            -- li-tasks with class completed (if it is), containing viewTask or editTask
            , H.ul
                [ Attr.class "todo-list" ]
                (filtered model.activeFilter model.tasks |> List.map (viewTask model.editingTask))
            ]


{-| should display an task
-}
viewTask : Maybe { id : Task.Id, text : String } -> Task -> Html Msg
viewTask editing task =
    let
        isEditing =
            Maybe.map .id editing == Just task.id
    in
    H.li
        [ Attr.classList
            [ ( "completed", task.completed )
            , ( "editing", isEditing )
            ]
        ]
        [ H.div
            [ Attr.class "view" ]
            [ H.input
                [ Attr.class "toggle"
                , Attr.type_ "checkbox"
                , Attr.checked task.completed
                , Ev.onCheck (CheckTask task.id)
                ]
                []
            , H.label
                [ Ev.onDoubleClick (EditTask task)
                ]
                [ H.text task.text ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteTask task.id)
                ]
                []
            ]
        , H.input
            [ Attr.class "edit"
            , Attr.id ("edit_" ++ Task.idToString task.id)
            , Attr.value (Maybe.map .text editing |> Maybe.withDefault "")
            , Ev.onInput UpdateEditText
            , onFinish FinishEdit CancelEdit
            , Ev.onBlur CancelEdit
            ]
            []
        ]


{-| should be hidden by default and shown when there are tasks
-}
viewFooter : Model -> Html Msg
viewFooter model =
    if Tasks.isEmpty model.tasks then
        H.text ""

    else
        H.footer
            [ Attr.class "footer" ]
            [ H.span [ Attr.class "todo-count" ]
                [ H.strong [] [ H.text (String.fromInt (model.tasks |> activeCount)) ]
                , H.text " task left"
                ]
            , H.ul [ Attr.class "filters" ]
                [ H.li []
                    [ H.button
                        [ Attr.classList [ ( "selected", model.activeFilter == All ) ]
                        ]
                        [ H.text "All" ]
                    ]
                , H.li []
                    [ H.button
                        [ Attr.classList [ ( "selected", model.activeFilter == Active ) ]
                        ]
                        [ H.text "Active" ]
                    ]
                , H.li []
                    [ H.button
                        [ Attr.classList [ ( "selected", model.activeFilter == Completed ) ]
                        ]
                        [ H.text "Completed" ]
                    ]
                ]
            , H.button
                [ Attr.class "clear-completed"
                , Ev.onClick ClearCompleted
                ]
                [ H.text "Clear completed" ]
            ]


onFinish : Msg -> Msg -> H.Attribute Msg
onFinish enterMessage escapeMessage =
    let
        select key =
            case key of
                13 ->
                    enterMessage

                27 ->
                    escapeMessage

                _ ->
                    -- Not a 'finish' key, such as ENTER or ESCAPE
                    NoOp
    in
    Ev.on "keydown" (Json.map select Ev.keyCode)
