module Util exposing (..)

import Types exposing (..)


fileFromJust : Maybe File -> File
fileFromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            { name = ""
            , issues = []
            , scripture = []
            , outline = ""
            , lists = ""
            , tables = []
            , quotes = []
            , asides = []
            }


navFromJust : Maybe NavLink -> NavLink
navFromJust x =
    case x of
        Just a ->
            a

        Nothing ->
            { href = "/#/"
            , id = "nothing-9999"
            , isActive = False
            , stage = ListInspect
            , text = ""
            }


getSingleNavByStage : Stage -> List NavLink -> NavLink
getSingleNavByStage stage navLinks =
    navLinks
        |> List.filter (\data -> data.stage == stage)
        |> List.head
        |> navFromJust


getSingleNavById : String -> List NavLink -> NavLink
getSingleNavById id navLinks =
    navLinks
        |> List.filter (\data -> data.id == id)
        |> List.head
        |> navFromJust


getSingleNavByHref : String -> List NavLink -> NavLink
getSingleNavByHref href navLinks =
    navLinks
        |> List.filter (\data -> data.href == href)
        |> List.head
        |> navFromJust


hasOutline : File -> Bool
hasOutline data =
    not (String.isEmpty data.outline)


hasLists : File -> Bool
hasLists data =
    not (String.isEmpty data.lists)


hasIssues : File -> Bool
hasIssues data =
    not (List.isEmpty data.issues)


hasScripIssues : File -> Bool
hasScripIssues data =
    not (List.isEmpty data.scripture)


hasTables : File -> Bool
hasTables data =
    not (List.isEmpty data.tables)


hasQuotes : File -> Bool
hasQuotes data =
    not (List.isEmpty data.quotes)


hasAsides : File -> Bool
hasAsides data =
    not (List.isEmpty data.asides)


isSelected : Maybe Name -> File -> Bool
isSelected name file =
    case name of
        Nothing ->
            False

        Just x ->
            x == file.name


issueCount : File -> Int
issueCount data =
    List.length data.issues


scripIssueCount : File -> Int
scripIssueCount data =
    List.length data.scripture


stageToTitle : Stage -> String
stageToTitle stage =
    case stage of
        Structure ->
            "Structural Issues"

        FileIssues ->
            "Markup Issues"

        ScripIssues ->
            "Scripture Issues"

        HeadOutline ->
            "Heading Outlines"

        ListInspect ->
            "Lists"

        TableInspect ->
            "Tables"

        QuoteInspect ->
            "Quotes"

        AsideInspect ->
            "Asides"

        TocDiff ->
            "TOC"


stageToIcon : Stage -> String
stageToIcon stage =
    case stage of
        Structure ->
            "icon-folder"

        FileIssues ->
            "fa fa-file-code-o"

        ScripIssues ->
            "fa fa-book"

        HeadOutline ->
            "fa fa-header"

        ListInspect ->
            "icon-list"

        TableInspect ->
            "fa fa-table"

        QuoteInspect ->
            "fa fa-quote-left"

        AsideInspect ->
            "fa fa-file-text-o"

        TocDiff ->
            "fa fa-indent"


toValidCount : Int -> Maybe Int
toValidCount errCount =
    if errCount > 0 then
        Just errCount
    else
        Nothing
