module Update exposing (..)

import Array exposing (..)
import Regex exposing (..)
import Routing exposing (parseLocation)
import Types exposing (..)
import Util exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadData (Ok data) ->
            ( { model | reportData = data, navLinks = initNavLinks data }, Cmd.none )

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

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                navLink =
                    getSingleNavByHref ("/" ++ location.hash) model.navLinks

                prevId =
                    model.activeLinkId

                navData =
                    if not (prevId == navLink.id) then
                        model.navLinks
                            |> reviseActive prevId navLink.id
                    else
                        model.navLinks
            in
            ( { model
                | route = newRoute
                , location = location
                , activeLinkId = navLink.id
                , navLinks = navData
              }
            , Cmd.none
            )

        SetActiveLink id ->
            let
                prevId =
                    model.activeLinkId

                navData =
                    if not (prevId == id) then
                        model.navLinks
                            |> reviseActive prevId id
                    else
                        model.navLinks
            in
            ( { model
                | activeLinkId = id
                , navLinks = navData
              }
            , Cmd.none
            )

        ToggleSidebar ->
            ( { model | sidebarMinimized = not model.sidebarMinimized }, Cmd.none )

        ToggleMobileSide ->
            ( { model | sidebarMobileShow = not model.sidebarMobileShow }, Cmd.none )

        ToggleHeadDrop ->
            ( { model
                | headDropIsOpen = not model.headDropIsOpen
                , listDropIsOpen =
                    if model.listDropIsOpen && not model.headDropIsOpen then
                        False
                    else
                        model.listDropIsOpen
                , tableDropIsOpen =
                    if model.tableDropIsOpen && not model.headDropIsOpen then
                        False
                    else
                        model.tableDropIsOpen
                , quoteDropIsOpen =
                    if model.quoteDropIsOpen && not model.headDropIsOpen then
                        False
                    else
                        model.quoteDropIsOpen
                , asideDropIsOpen =
                    if model.asideDropIsOpen && not model.headDropIsOpen then
                        False
                    else
                        model.asideDropIsOpen
              }
            , Cmd.none
            )

        ToggleListDrop ->
            ( { model
                | listDropIsOpen = not model.listDropIsOpen
                , headDropIsOpen =
                    if model.headDropIsOpen && not model.listDropIsOpen then
                        False
                    else
                        model.headDropIsOpen
                , tableDropIsOpen =
                    if model.tableDropIsOpen && not model.listDropIsOpen then
                        False
                    else
                        model.tableDropIsOpen
                , quoteDropIsOpen =
                    if model.quoteDropIsOpen && not model.listDropIsOpen then
                        False
                    else
                        model.quoteDropIsOpen
                , asideDropIsOpen =
                    if model.asideDropIsOpen && not model.listDropIsOpen then
                        False
                    else
                        model.asideDropIsOpen
              }
            , Cmd.none
            )

        ToggleTableDrop ->
            ( { model
                | tableDropIsOpen = not model.tableDropIsOpen
                , headDropIsOpen =
                    if model.headDropIsOpen && not model.tableDropIsOpen then
                        False
                    else
                        model.headDropIsOpen
                , listDropIsOpen =
                    if model.listDropIsOpen && not model.tableDropIsOpen then
                        False
                    else
                        model.listDropIsOpen
                , quoteDropIsOpen =
                    if model.quoteDropIsOpen && not model.tableDropIsOpen then
                        False
                    else
                        model.quoteDropIsOpen
                , asideDropIsOpen =
                    if model.asideDropIsOpen && not model.tableDropIsOpen then
                        False
                    else
                        model.asideDropIsOpen
              }
            , Cmd.none
            )

        ToggleQuoteDrop ->
            ( { model
                | quoteDropIsOpen = not model.quoteDropIsOpen
                , headDropIsOpen =
                    if model.headDropIsOpen && not model.quoteDropIsOpen then
                        False
                    else
                        model.headDropIsOpen
                , listDropIsOpen =
                    if model.listDropIsOpen && not model.quoteDropIsOpen then
                        False
                    else
                        model.listDropIsOpen
                , tableDropIsOpen =
                    if model.tableDropIsOpen && not model.quoteDropIsOpen then
                        False
                    else
                        model.tableDropIsOpen
                , asideDropIsOpen =
                    if model.asideDropIsOpen && not model.quoteDropIsOpen then
                        False
                    else
                        model.asideDropIsOpen
              }
            , Cmd.none
            )

        ToggleAsideDrop ->
            ( { model
                | asideDropIsOpen = not model.asideDropIsOpen
                , headDropIsOpen =
                    if model.headDropIsOpen && not model.asideDropIsOpen then
                        False
                    else
                        model.headDropIsOpen
                , listDropIsOpen =
                    if model.listDropIsOpen && not model.asideDropIsOpen then
                        False
                    else
                        model.listDropIsOpen
                , quoteDropIsOpen =
                    if model.quoteDropIsOpen && not model.asideDropIsOpen then
                        False
                    else
                        model.quoteDropIsOpen
                , tableDropIsOpen =
                    if model.tableDropIsOpen && not model.asideDropIsOpen then
                        False
                    else
                        model.tableDropIsOpen
              }
            , Cmd.none
            )


fileToNavLink : Int -> File -> Stage -> NavLink
fileToNavLink index file stage =
    let
        linkText =
            file.name |> replace All (regex "^[^_]+_[^_]+_") (\_ -> "")

        sectionType =
            case stage of
                HeadOutline ->
                    "outline"

                ListInspect ->
                    "lists"

                TableInspect ->
                    "tables"

                QuoteInspect ->
                    "quotes"

                AsideInspect ->
                    "asides"

                _ ->
                    ""
    in
    { href = "/#/" ++ file.name ++ "/" ++ sectionType
    , id = "nav-" ++ sectionType ++ "-" ++ toString (index + 1)
    , isActive = False
    , stage = stage
    , text = linkText
    }


getListOfNavLinks : List File -> Stage -> List NavLink
getListOfNavLinks files stage =
    Array.fromList files
        |> indexedMap (\index file -> fileToNavLink index file stage)
        |> toList


initNavLinks : FileData -> List NavLink
initNavLinks reportData =
    let
        files =
            reportData.files

        filesWithOutline =
            List.filter hasOutline files

        filesWithLists =
            List.filter hasLists files

        filesWithTables =
            List.filter hasTables files

        filesWithQuotes =
            List.filter hasQuotes files

        filesWithAsides =
            List.filter hasAsides files

        staticNavLinks =
            [ { href = "/#/issues/structural"
              , id = "nav-structure-issues"
              , isActive = False
              , stage = Structure
              , text = "Structural Issues"
              }
            , { href = "/#/issues/markup"
              , id = "nav-markup-issues"
              , isActive = False
              , stage = FileIssues
              , text = "Markup Issues"
              }
            , { href = "/#/issues/scripture"
              , id = "nav-scrip-issues"
              , isActive = False
              , stage = ScripIssues
              , text = "Scripture Issues"
              }
            , { href = "/#/toc"
              , id = "nav-toc-diff"
              , isActive = False
              , stage = TocDiff
              , text = "TOC"
              }
            ]
    in
    staticNavLinks
        |> List.append (getListOfNavLinks filesWithOutline HeadOutline)
        |> List.append (getListOfNavLinks filesWithLists ListInspect)
        |> List.append (getListOfNavLinks filesWithTables TableInspect)
        |> List.append (getListOfNavLinks filesWithQuotes QuoteInspect)
        |> List.append (getListOfNavLinks filesWithAsides AsideInspect)


reviseActive : String -> String -> List NavLink -> List NavLink
reviseActive prevId newId navLinks =
    let
        toggleIds : NavLink -> NavLink
        toggleIds navLink =
            if navLink.id == prevId then
                { navLink | isActive = False }
            else if navLink.id == newId then
                { navLink | isActive = True }
            else
                navLink
    in
    navLinks
        |> List.map toggleIds
