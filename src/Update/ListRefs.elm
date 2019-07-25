module Update.ListRefs exposing (listRefs)

import Types exposing (..)
import Utils exposing (..)


listRefs : Maybe RefType -> Model -> ( Model, Cmd Msg )
listRefs mayRefType model =
    let
        newModel =
            if mayRefType == model.selectedRefType then
                { model | selectedRefType = Nothing }

            else
                { model | selectedRefType = mayRefType }

        listedRefArray =
            getListedRefArray newModel
    in
    { newModel | listedRefIds = listedRefArray } ! []
