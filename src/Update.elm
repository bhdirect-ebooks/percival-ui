module Update exposing (..)

import Types exposing (..)
import Utils exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadData (Ok data) ->
            ( { model
                | percivalData = data
                , currentDocId = getFirstIdOfDict data.docs
              }
            , Cmd.none
            )

        LoadData (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            ( { model
                | loadingError = Just "Something went wrong. Shall we play a game?"
              }
            , Cmd.none
            )

        ToNextDoc ->
            ( { model | currentDocId = getNextDocId model }, Cmd.none )

        ToPrevDoc ->
            ( { model | currentDocId = getPrevDocId model }, Cmd.none )

        KeyDown keyCode ->
            if not model.inEditMode then
                case keyCode of
                    -- q
                    81 ->
                        model |> update ToPrevDoc

                    -- w
                    87 ->
                        model |> update ToNextDoc

                    _ ->
                        ( model, Cmd.none )
            else
                ( model, Cmd.none )
