module Update.Load exposing (load, loadErr)

import Regex
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


load : PercivalData -> Model -> ( Model, Cmd Msg )
load data model =
    let
        newModel =
            { model
                | volumeTitle = data.volumeTitle
                , parserOpts = data.parserOpts
                , docs = data.docs
                , blockState =
                    UndoList.fresh
                        { changedBlockIds = []
                        , blocks = data.blocks
                        }
                , currentDocId = getFirstIdOfDict data.docs
            }

        docRefArray =
            getDocRefArray newModel

        dashboard =
            getDashboard newModel data.trueDocs
    in
    { newModel
        | docRefIds = docRefArray
        , listedRefIds = docRefArray
        , dashboard = dashboard
    }
        ! []


loadErr : a -> Model -> ( Model, Cmd Msg )
loadErr err model =
    let
        _ =
            Debug.log "Err" err

        showError =
            Regex.replace (Regex.AtMost 1)
                (Regex.regex "^(.*?), body = .*?$")
                (\{ submatches } ->
                    case submatches of
                        first :: _ ->
                            Maybe.withDefault "" first

                        _ ->
                            ""
                )
                (toString err)
    in
    { model | loadingError = Just showError } ! []
