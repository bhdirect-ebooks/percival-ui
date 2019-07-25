port module Update.SelectText exposing
    ( clearTextSelection
    , deselect
    , handleTextParserSuccess
    , handleTextSelection
    , parseSelection
    , textSelected
    , trySelection
    )

import Dict
import Dom
import Regex exposing (..)
import ServerIO exposing (postTextSelection)
import Task
import Types exposing (..)
import Update.SubmitHtml exposing (submitHtml)


port trySelection : Bool -> Cmd msg


port textSelected : (Selection -> msg) -> Sub msg


port deselect : Bool -> Cmd msg


clearTextSelection : Model -> Model
clearTextSelection model =
    { model
        | selection =
            { blockId = ""
            , selectedText = ""
            , anchorOffset = 0
            , focusOffset = 0
            , textContent = ""
            }
    }


handleTextSelection : Selection -> Model -> ( Model, Cmd Msg )
handleTextSelection selection model =
    { model | selection = selection } ! []


parseSelection : Model -> ( Model, Cmd Msg )
parseSelection model =
    { model
        | editingBlockId = model.selection.blockId
        , inEditMode = False
        , badInput = False
    }
        ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "context-field")
          , postTextSelection model.selection.selectedText model.contextField model.parserOpts
          ]


replaceSelection : String -> String -> Selection -> String
replaceSelection html taggedSelection selection =
    Regex.replace All
        (regex (escape selection.textContent))
        (\match ->
            Regex.replace All
                (regex (escape selection.selectedText))
                (\innerMatch ->
                    if selection.anchorOffset == innerMatch.index then
                        taggedSelection

                    else
                        innerMatch.match
                )
                match.match
        )
        html


handleTextParserSuccess : ParsedText -> Model -> ( Model, Cmd Msg )
handleTextParserSuccess { parsedText } model =
    let
        block =
            Dict.get model.editingBlockId model.blockState.present.blocks

        newHtml =
            case block of
                Nothing ->
                    ""

                Just block ->
                    replaceSelection block.html parsedText model.selection

        newModel =
            { model | htmlSource = newHtml, contextField = "" }
    in
    submitHtml Trusted newModel
