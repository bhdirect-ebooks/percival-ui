module Main exposing (..)

import Decoders exposing (decodePercivalData)
import Dict
import Html
import Http
import Keyboard exposing (downs)
import Types exposing (..)
import Update exposing (..)
import View exposing (viewOrError)


initialModel : Model
initialModel =
    { percivalData =
        { volumeTitle = ""
        , parserOpts =
            { versification = ""
            , language = ""
            }
        , docs = Dict.fromList []
        , blocks = Dict.fromList []
        }
    , currentDocId = ""
    , currentRefId = ""
    , loadingError = Nothing
    , isSaving = False
    , inEditMode = False
    }


initialCmd : Cmd Msg
initialCmd =
    decodePercivalData
        |> Http.get "http://localhost:7777/data/"
        |> Http.send LoadData


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ downs KeyDown ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = subscriptions
        }
