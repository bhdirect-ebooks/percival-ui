module Update.OsisField exposing (changeOsis, editOsis, handleParserError, handleParserSuccess, updateField)

import Dom
import ServerIO exposing (postFieldInput)
import Task
import Types exposing (..)
import Utils exposing (..)


editOsis : Model -> ( Model, Cmd Msg )
editOsis model =
    { model
        | inEditMode = True
        , editingOsis = True
    }
        ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]


updateField : String -> Model -> ( Model, Cmd Msg )
updateField str model =
    { model | osisField = str } ! []


changeOsis : Model -> ( Model, Cmd Msg )
changeOsis model =
    let
        currentOsisOrMess =
            getOsisWithRefId model.currentRefId model.blockState.present.blocks
    in
    if model.osisField == "" then
        { model | badInput = True } ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]

    else if not (model.osisField == currentOsisOrMess) then
        { model
            | inEditMode = False
            , editingOsis = False
            , badInput = False
            , viewScriptureText = False
            , scriptureText = ""
        }
            ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "osis-field")
              , postFieldInput model.osisField model.parserOpts
              ]

    else
        { model
            | inEditMode = False
            , editingOsis = False
            , badInput = False
        }
            ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "osis-field") ]


handleParserError : a -> Model -> ( Model, Cmd Msg )
handleParserError err model =
    let
        _ =
            Debug.log "Err" err
    in
    { model | badInput = True } ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]


handleParserSuccess : RefData -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleParserSuccess refData ( model, cmd ) =
    let
        domCmd =
            if refData.valid then
                Dom.blur "osis-field"

            else
                Dom.focus "osis-field"

        commands =
            [ cmd ]
                |> List.append [ Task.attempt (\_ -> DoNothing) domCmd ]
    in
    model ! commands
