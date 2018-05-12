module DropdownMenu
    exposing
        ( Input
        , Msg
        , State
        , autocomplete
        , closed
        , config
        , update
        , view
        )

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Task


type State a
    = State
        { open : Bool
        , preventBlur : Bool
        , preventInput : Bool
        , query : String
        , keyboardFocus : Maybe Int
        , mouseFocus : Maybe Int
        , scrollDataCache : Maybe ScrollData
        }



---- CONSTRUCTOR


closed : State a
closed =
    State
        { open = False
        , preventBlur = False
        , preventInput = False
        , query = ""
        , keyboardFocus = Nothing
        , mouseFocus = Nothing
        , scrollDataCache = Nothing
        }



---- CONFIGURATION


type Config a
    = Config
        { input : Input a
        , wrapper : List (Html.Attribute Never)
        , list : List (Html.Attribute Never)
        , entry : Bool -> Bool -> Bool -> a -> HtmlDetails
        , entryId : a -> String
        , wrapKeyboardFocus : Bool
        }


type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }


config :
    { input : Input a
    , wrapper : List (Html.Attribute Never)
    , list : List (Html.Attribute Never)
    , entry : Bool -> Bool -> Bool -> a -> HtmlDetails
    , entryId : a -> String
    , wrapKeyboardFocus : Bool
    }
    -> Config a
config { input, wrapper, list, entry, entryId, wrapKeyboardFocus } =
    Config
        { input = input
        , wrapper = wrapper
        , list = list
        , entry = entry
        , entryId = entryId
        , wrapKeyboardFocus = wrapKeyboardFocus
        }


type Input a
    = Autocomplete
        { textfield : Maybe a -> Bool -> List (Html.Attribute Never)
        , matchesQuery : String -> a -> Bool
        , printSelection : a -> String
        }


autocomplete :
    { textfield : Maybe a -> Bool -> List (Html.Attribute Never)
    , matchesQuery : String -> a -> Bool
    , printSelection : a -> String
    }
    -> Input a
autocomplete { textfield, matchesQuery, printSelection } =
    Autocomplete
        { textfield = textfield
        , matchesQuery = matchesQuery
        , printSelection = printSelection
        }



---- VIEW


view :
    Config a
    ->
        { id : String
        , labelledBy : String
        }
    -> State a
    -> Maybe a
    -> List a
    -> Html (Msg a)
view (Config cfg) { id, labelledBy } (State stuff) maybeSelection entries =
    let
        filteredEntries =
            case cfg.input of
                Autocomplete { matchesQuery } ->
                    entries
                        |> List.filter (matchesQuery stuff.query)

        keyboardFocusedEntry =
            stuff.keyboardFocus
                |> Maybe.andThen
                    (\index ->
                        filteredEntries
                            |> List.drop index
                            |> List.head
                    )
    in
    Html.div
        (appendAttributes cfg.wrapper [])
        [ viewInput
            cfg.input
            id
            labelledBy
            cfg.wrapKeyboardFocus
            maybeSelection
            keyboardFocusedEntry
            stuff.keyboardFocus
            stuff.query
            stuff.open
            (List.length filteredEntries)
        , viewEntries
            cfg.list
            cfg.entry
            cfg.entryId
            id
            labelledBy
            maybeSelection
            stuff.keyboardFocus
            stuff.mouseFocus
            stuff.open
            filteredEntries
        ]


viewInput :
    Input a
    -> String
    -> String
    -> Bool
    -> Maybe a
    -> Maybe a
    -> Maybe Int
    -> String
    -> Bool
    -> Int
    -> Html (Msg a)
viewInput input id labelledBy wrapKeyboardFocus maybeSelection keyboardFocusedEntry keyboardFocus query open entriesCount =
    let
        setAriaExpanded attrs =
            if open then
                Attributes.attribute "aria-expanded" "true" :: attrs
            else
                attrs

        scrollDataDecoder otherIndex =
            Decode.succeed ScrollData
                |> Decode.requiredAt [ "target", "nextSibling", "scrollTop" ] Decode.float
                |> Decode.requiredAt [ "target", "nextSibling", "clientHeight" ] Decode.float
                |> Decode.requiredAt
                    [ "target"
                    , "nextSibling"
                    , "childNodes"
                    , String.fromInt otherIndex
                    , "offsetTop"
                    ]
                    Decode.float
                |> Decode.requiredAt
                    [ "target"
                    , "nextSibling"
                    , "childNodes"
                    , String.fromInt otherIndex
                    , "offsetHeight"
                    ]
                    Decode.float
    in
    case input of
        Autocomplete { textfield, printSelection } ->
            Html.input
                ([ Events.onClick (TextfieldClicked id)
                 , Events.onInput (TextfieldChanged id)
                 , Events.on "keydown"
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    "ArrowUp" ->
                                        let
                                            newIndex =
                                                case keyboardFocus of
                                                    Nothing ->
                                                        entriesCount - 1

                                                    Just index ->
                                                        if wrapKeyboardFocus && index == 0 then
                                                            entriesCount - 1
                                                        else
                                                            clamp 0 (entriesCount - 1) <|
                                                                (index - 1)
                                        in
                                        Decode.map (ArrowUpPressed id newIndex)
                                            (scrollDataDecoder newIndex)

                                    "ArrowDown" ->
                                        let
                                            newIndex =
                                                case keyboardFocus of
                                                    Nothing ->
                                                        0

                                                    Just index ->
                                                        if
                                                            wrapKeyboardFocus
                                                                && (index == entriesCount - 1)
                                                        then
                                                            0
                                                        else
                                                            clamp 0 (entriesCount - 1) <|
                                                                (index + 1)
                                        in
                                        Decode.map (ArrowDownPressed id newIndex)
                                            (scrollDataDecoder newIndex)

                                    " " ->
                                        Decode.succeed (SpacePressed id)

                                    "Enter" ->
                                        case keyboardFocusedEntry of
                                            Just entry ->
                                                Decode.succeed (EnterPressed entry)

                                            Nothing ->
                                                Decode.fail "not handling that key here"

                                    "Escape" ->
                                        Decode.succeed EscapePressed

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
                 , Events.on "keyup"
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    " " ->
                                        Decode.succeed SpaceReleased

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
                 , Events.on "blur" <|
                    case keyboardFocus of
                        Nothing ->
                            Decode.succeed (TextfieldBlured Nothing)

                        Just index ->
                            Decode.map (TextfieldBlured << Just)
                                (scrollDataDecoder index)
                 , Attributes.style "position" "relative"
                 , Attributes.value query
                 , maybeSelection
                    |> Maybe.map printSelection
                    |> Maybe.withDefault ""
                    |> Attributes.placeholder
                 , Attributes.tabindex 0
                 , Attributes.id (id ++ "__textfield")
                 , Attributes.attribute "aria-haspopup" "listbox"
                 , Attributes.attribute "aria-labelledby" (id ++ "__textfield" ++ " " ++ labelledBy)
                 ]
                    |> setAriaExpanded
                    |> appendAttributes (textfield maybeSelection open)
                )
                []


viewEntries :
    List (Html.Attribute Never)
    -> (Bool -> Bool -> Bool -> a -> HtmlDetails)
    -> (a -> String)
    -> String
    -> String
    -> Maybe a
    -> Maybe Int
    -> Maybe Int
    -> Bool
    -> List a
    -> Html (Msg a)
viewEntries list entry entryId id labelledBy maybeSelection keyboardFocus mouseFocus open entries =
    let
        setDisplay attrs =
            if open then
                attrs
            else
                Attributes.style "display" "none" :: attrs

        setAriaActivedescendant attrs =
            case keyboardFocus of
                Nothing ->
                    attrs

                Just index ->
                    entries
                        |> List.drop index
                        |> List.head
                        |> Maybe.map
                            (\a ->
                                Attributes.attribute "aria-activedescendant"
                                    (id ++ "__element--" ++ entryId a)
                                    :: attrs
                            )
                        |> Maybe.withDefault attrs
    in
    Html.ul
        ([ Attributes.style "position" "absolute"
         , Events.on "mousedown" (Decode.succeed MenuMouseDown)
         , Events.on "mouseup" (Decode.succeed MenuMouseUp)
         , Attributes.id (id ++ "__element-list")
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" labelledBy
         ]
            |> setDisplay
            |> setAriaActivedescendant
            |> appendAttributes list
        )
        (List.indexedMap
            (\index a ->
                viewEntry
                    entry
                    entryId
                    id
                    maybeSelection
                    (keyboardFocus == Just index)
                    (mouseFocus == Just index)
                    index
                    a
            )
            entries
        )


viewEntry :
    (Bool -> Bool -> Bool -> a -> HtmlDetails)
    -> (a -> String)
    -> String
    -> Maybe a
    -> Bool
    -> Bool
    -> Int
    -> a
    -> Html (Msg a)
viewEntry entry entryId id selection keyboardFocused mouseFocused index a =
    let
        selected =
            selection == Just a

        { attributes, children } =
            entry selected keyboardFocused mouseFocused a
    in
    Html.li
        ([ Events.onMouseEnter (EntryMouseEntered index)
         , Events.onMouseLeave EntryMouseLeft
         , Events.onClick (EntryClicked a)
         , Attributes.id (id ++ "__element--" ++ entryId a)
         , Attributes.attribute "role" "option"
         ]
            |> appendAttributes attributes
        )
        (children
            |> List.map (Html.map (\_ -> NoOp))
        )


appendAttributes :
    List (Html.Attribute Never)
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
appendAttributes neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> NoOp))
        |> List.append attrs



---- UPDATE


type Msg a
    = NoOp
      -- TEXTFIELD
    | TextfieldClicked String
    | TextfieldBlured (Maybe ScrollData)
    | TextfieldChanged String String
    | ArrowUpPressed String Int ScrollData
    | ArrowDownPressed String Int ScrollData
    | SpacePressed String
    | SpaceReleased
    | EnterPressed a
    | EscapePressed
      -- MENU
    | MenuMouseDown
    | MenuMouseUp
      -- ENTRY
    | EntryMouseEntered Int
    | EntryMouseLeft
    | EntryClicked a


type alias ScrollData =
    { listScrollTop : Float
    , listClientHeight : Float
    , entryOffsetTop : Float
    , entryOffsetHeight : Float
    }


update :
    { entrySelected : a -> msg
    , selectionDismissed : msg
    }
    -> Maybe a
    -> State a
    -> Msg a
    -> ( State a, Cmd (Msg a), Maybe msg )
update { entrySelected, selectionDismissed } maybeSelection ((State stuff) as state) msg =
    case msg of
        NoOp ->
            ( state, Cmd.none, Nothing )

        -- TEXTFIELD
        TextfieldClicked id ->
            ( State
                { stuff
                    | open = not stuff.open
                    , scrollDataCache = Nothing
                }
            , case stuff.scrollDataCache of
                Nothing ->
                    Browser.setScrollTop (id ++ "__element-list") 0
                        |> Task.attempt (\_ -> NoOp)

                Just scrollData ->
                    case stuff.keyboardFocus of
                        Nothing ->
                            Browser.setScrollTop (id ++ "__element-list") 0
                                |> Task.attempt (\_ -> NoOp)

                        Just _ ->
                            adjustScrollTop id scrollData
            , Nothing
            )

        TextfieldBlured scrollDataCache ->
            ( State
                { stuff
                    | open =
                        if stuff.preventBlur then
                            stuff.open
                        else
                            False
                    , scrollDataCache = scrollDataCache
                }
            , Cmd.none
            , Nothing
            )

        TextfieldChanged id newQuery ->
            if stuff.preventInput then
                ( state, Cmd.none, Nothing )
            else
                ( State
                    { stuff
                        | open = True
                        , query = newQuery
                        , keyboardFocus = Nothing
                    }
                , Browser.setScrollTop (id ++ "__element-list") 0
                    |> Task.attempt (\_ -> NoOp)
                , Nothing
                )

        ArrowUpPressed id index scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just index
                    , open = True
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        ArrowDownPressed id index scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just index
                    , open = True
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        SpacePressed id ->
            ( State
                { stuff
                    | open = True
                    , preventInput =
                        if stuff.open then
                            stuff.preventInput
                        else
                            True
                }
            , case stuff.scrollDataCache of
                Nothing ->
                    Browser.setScrollTop (id ++ "__element-list") 0
                        |> Task.attempt (\_ -> NoOp)

                Just scrollData ->
                    case stuff.keyboardFocus of
                        Nothing ->
                            Browser.setScrollTop (id ++ "__element-list") 0
                                |> Task.attempt (\_ -> NoOp)

                        Just _ ->
                            adjustScrollTop id scrollData
            , Nothing
            )

        SpaceReleased ->
            ( State
                { stuff
                    | open = True
                    , preventInput = False
                }
            , Cmd.none
            , Nothing
            )

        EnterPressed a ->
            ( State
                { stuff
                    | open = False
                    , query = ""
                    , keyboardFocus = Nothing
                }
            , Cmd.none
            , Just (entrySelected a)
            )

        EscapePressed ->
            ( State
                { stuff
                    | open = False
                    , query = ""
                }
            , Cmd.none
            , Nothing
            )

        -- MENU
        MenuMouseDown ->
            ( State { stuff | preventBlur = True }
            , Cmd.none
            , Nothing
            )

        MenuMouseUp ->
            ( State { stuff | preventBlur = False }
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered index ->
            ( State
                { stuff | mouseFocus = Just index }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( State
                { stuff | mouseFocus = Nothing }
            , Cmd.none
            , Nothing
            )

        EntryClicked a ->
            ( State
                { stuff
                    | open = False
                    , query = ""
                    , keyboardFocus = Nothing
                }
            , Cmd.none
            , Just (entrySelected a)
            )


adjustScrollTop : String -> ScrollData -> Cmd (Msg a)
adjustScrollTop id { listScrollTop, listClientHeight, entryOffsetTop, entryOffsetHeight } =
    if (entryOffsetTop + entryOffsetHeight) > (listScrollTop + listClientHeight) then
        Browser.setScrollTop (id ++ "__element-list")
            (entryOffsetTop + entryOffsetHeight - listClientHeight)
            |> Task.attempt (\_ -> NoOp)
    else if entryOffsetTop < listScrollTop then
        Browser.setScrollTop (id ++ "__element-list") entryOffsetTop
            |> Task.attempt (\_ -> NoOp)
    else
        Cmd.none
