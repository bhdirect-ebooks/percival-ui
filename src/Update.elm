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
                | loadingError = Just "Error! (Try turning it off and on again?)"
              }
            , Cmd.none
            )
