module Update exposing (..)

import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadData (Ok data) ->
            ( { model | percivalData = data }, Cmd.none )

        LoadData (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            ( { model
                | loadingError = Just "Well, this is embarrassing. Something went wrong."
              }
            , Cmd.none
            )
