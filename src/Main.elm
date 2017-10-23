module Main exposing (..)

import Decoders exposing (decodeFileData)
import Http
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
        , docs = []
        }
    , currentDoc = ""
    , currentRef = ""
    , loadingError = Nothing
    , isSaving = False
    , inEditMode = False
    }


initialCmd : Cmd Msg
initialCmd =
    decodeFileData
        |> Http.get "http://localhost:7777/data/"
        |> Http.send LoadData


main : Program Never Model Msg
main =
    { init = ( initialModel, initialCmd )
    , view = viewOrError
    , update = update
    , subscriptions = \_ -> Sub.none
    }
