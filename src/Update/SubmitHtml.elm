module Update.SubmitHtml exposing (submitHtml)

import Dom
import ServerIO exposing (..)
import Task
import Types exposing (..)


submitHtml : HtmlTrust -> Model -> ( Model, Cmd Msg )
submitHtml trust model =
    if model.htmlSource == "" then
        { model | htmlValidation = [ "Cannot be empty." ] }
            ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]

    else
        case trust of
            Trusted ->
                { model | isSaving = True } ! [ postNewHtml model.editingBlockId model.htmlSource ]

            Untrusted ->
                { model | isValidating = True } ! [ postValidateHtml model.htmlSource ]
