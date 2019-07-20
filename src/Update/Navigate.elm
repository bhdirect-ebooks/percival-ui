port module Update.Navigate exposing
    ( backtoTop
    , scrollDoc
    , scrollList
    , toDoc
    , toDocFromDash
    , toRef
    )

import Task
import Types exposing (..)
import Utils exposing (..)


port backtoTop : Bool -> Cmd msg


port scrollList : String -> Cmd msg


port scrollDoc : String -> Cmd msg


toDocFromDash : String -> Model -> ( Model, Cmd Msg )
toDocFromDash docId model =
    { model | showDash = False } ! [ Task.succeed (ToDoc (ById docId)) |> Task.perform identity ]


toDoc : DocNav -> Model -> ( Model, Cmd Msg )
toDoc navData model =
    let
        selectedDoc =
            case navData of
                ByDir dir ->
                    getNearbyDocId dir model

                ById docId ->
                    docId
    in
    if selectedDoc == model.currentDocId || model.inEditMode then
        model ! []

    else
        let
            newModel =
                { model
                    | currentDocId = selectedDoc
                    , selectedRefType = Nothing
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                }

            docRefArray =
                getDocRefArray newModel
        in
        { newModel
            | docRefIds = docRefArray
            , listedRefIds = docRefArray
        }
            ! [ backtoTop True ]


toRef : NavDir -> Maybe Confirmation -> Model -> ( Model, Cmd Msg )
toRef dir mayConfirm model =
    let
        selectedRef =
            case mayConfirm of
                Nothing ->
                    getNearbyRefId dir model.currentRefId model.listedRefIds

                Just conf ->
                    case conf of
                        Unconfirmed ->
                            getNearbyUnconfId dir model.currentRefId model.listedRefIds

                        _ ->
                            model.currentRefId

        refListId =
            getRefListId selectedRef

        blockId =
            getBlockIdFromRefId selectedRef

        osisOrMessage =
            getOsisWithRefId selectedRef model.blockState.present.blocks
    in
    if model.currentRefId == selectedRef || model.inEditMode then
        model ! []

    else
        { model
            | currentRefId = selectedRef
            , viewAltRefs = False
            , viewScriptureText = False
            , scriptureText = ""
            , osisField = osisOrMessage
        }
            ! [ scrollList refListId, scrollDoc selectedRef ]
