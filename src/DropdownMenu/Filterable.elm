module DropdownMenu.Filterable
    exposing
        ( Config
        , Msg
        , State
        , closed
        , config
        , update
        , view
        , viewLazy
        )

{-| -}

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Task


{-| -}
type State a
    = State
        { open : Bool
        , preventBlur : Bool
        , query : String

        -- FOCUS
        , keyboardFocus : Maybe Int
        , mouseFocus : Maybe Int

        -- DOM MEASUREMENTS
        , scrollDataCache : Maybe ScrollData
        , ulScrollTop : Float
        , ulClientHeight : Float
        }


type alias ScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


{-| -}
closed : State a
closed =
    State
        { open = False
        , preventBlur = False
        , query = ""
        , keyboardFocus = Nothing
        , mouseFocus = Nothing
        , scrollDataCache = Nothing
        , ulScrollTop = 0

        -- FIXME: When we click the textfield, we cannot get the height of the
        -- list as it has not been rendered yet. The simplest workaround is to
        -- just put a very high default clientHeight. Are there smarter ways of
        -- getting the height of the list just right after it is visible in the
        -- DOM?
        , ulClientHeight = 1000
        }



---- CONFIG


{-| -}
type Config a
    = Config (ConfigData a)


type alias ConfigData a =
    { matchesQuery : String -> a -> Bool
    , entryId : a -> String
    , printEntry : a -> String

    -- BEHAVIOUR
    , jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool

    -- VIEW
    , container : HtmlAttributes
    , textfield :
        { selection : Maybe a
        , open : Bool
        }
        -> HtmlAttributes
    , ul : HtmlAttributes
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        }
        -> a
        -> HtmlDetails
    }


{-| -}
type alias HtmlAttributes =
    List (Html.Attribute Never)


{-| -}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }


{-| -}
config : ConfigData a -> Config a
config =
    Config



---- VIEW


{-| -}
view :
    Config a
    ->
        { id : String
        , labelledBy : String
        , placeholder : String
        }
    -> Maybe a
    -> State a
    -> List a
    -> Html (Msg a)
view ((Config { matchesQuery }) as cfg) ids selection ((State { query }) as state) entries =
    let
        filteredEntries =
            entries
                |> List.filter (matchesQuery query)
    in
    viewHelp cfg ids selection state <|
        { entries = filteredEntries
        , entriesCount = List.length filteredEntries
        , dropped = 0
        , heightAbove = 0
        , heightBelow = 0
        }


{-| -}
viewLazy :
    (a -> Float)
    -> Config a
    ->
        { id : String
        , labelledBy : String
        , placeholder : String
        }
    -> Maybe a
    -> State a
    -> List a
    -> Html (Msg a)
viewLazy entryHeight ((Config { matchesQuery }) as cfg) ids selection ((State stuff) as state) entries =
    let
        filteredEntries =
            entries
                |> List.filter (matchesQuery stuff.query)

        { visibleEntries, dropped, heightAbove, heightBelow } =
            List.foldl
                (\entry data ->
                    if data.heightAbove < stuff.ulScrollTop - 200 then
                        { data
                            | dropped = data.dropped + 1
                            , heightAbove = data.heightAbove + entryHeight entry
                        }
                    else if
                        (data.heightAbove + data.heightVisible)
                            > (stuff.ulScrollTop + stuff.ulClientHeight + 200)
                    then
                        { data | heightBelow = data.heightBelow + entryHeight entry }
                    else
                        { data
                            | visibleEntries = entry :: data.visibleEntries
                            , heightVisible = data.heightVisible + entryHeight entry
                        }
                )
                { visibleEntries = []
                , dropped = 0
                , heightAbove = 0
                , heightVisible = 0
                , heightBelow = 0
                }
                filteredEntries
    in
    viewHelp cfg ids selection state <|
        { entries = List.reverse visibleEntries
        , entriesCount = List.length filteredEntries
        , dropped = dropped
        , heightAbove = heightAbove + min 0 (heightBelow - 200)
        , heightBelow = heightBelow + min 0 (heightAbove - 200)
        }


type alias VisibleEntries a =
    { entries : List a
    , entriesCount : Int
    , dropped : Int
    , heightAbove : Float
    , heightBelow : Float
    }


viewHelp :
    Config a
    ->
        { id : String
        , labelledBy : String
        , placeholder : String
        }
    -> Maybe a
    -> State a
    -> VisibleEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy, placeholder } selection (State stuff) visibleEntries =
    let
        { entries, entriesCount, dropped, heightAbove, heightBelow } =
            visibleEntries
    in
    Html.div
        (appendAttributes cfg.container [])
        [ Html.input
            ([ Attributes.id (printTextfieldId id)
             , Attributes.type_ "text"
             , Attributes.attribute "aria-haspopup" "listbox"
             , Attributes.attribute "aria-labelledby"
                (printTextfieldId id ++ " " ++ labelledBy)
             , Attributes.style "position" "relative"
             , Attributes.tabindex 0
             , selection
                |> Maybe.map cfg.printEntry
                |> Maybe.withDefault placeholder
                |> Attributes.placeholder
             , Attributes.value stuff.query
             , Events.onClick (TextfieldClicked id)
             , Events.onInput (TextfieldChanged id)
             , Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (handleKeydown id
                            cfg.jumpAtEnds
                            stuff.open
                            stuff.keyboardFocus
                            entries
                            entriesCount
                            dropped
                        )
                )
             , Events.on "blur" <|
                case stuff.keyboardFocus of
                    Nothing ->
                        Decode.succeed (TextfieldBlured Nothing)

                    Just index ->
                        Decode.map (TextfieldBlured << Just)
                            (scrollDataDecoder (index - dropped + 1))
             ]
                |> setAriaExpanded stuff.open
                |> appendAttributes
                    (cfg.textfield
                        { selection = selection
                        , open = stuff.open
                        }
                    )
            )
            []
        , Html.ul
            ([ Attributes.id (printListId id)
             , Attributes.attribute "role" "listbox"
             , Attributes.attribute "aria-labelledby" labelledBy
             , Attributes.style "position" "absolute"
             , Events.on "mousedown" (Decode.succeed (ListMouseDown id))
             , Events.on "mouseup" (Decode.succeed ListMouseUp)
             , Events.on "scroll" <|
                Decode.map2 ListScrolled
                    (Decode.at [ "target", "scrollTop" ] Decode.float)
                    (Decode.at [ "target", "clientHeight" ] Decode.float)
             ]
                |> setDisplay stuff.open
                |> setAriaActivedescendant
                    (\a -> printEntryId id (cfg.entryId a))
                    (Maybe.map (\index -> index - dropped) stuff.keyboardFocus)
                    entries
                |> appendAttributes cfg.ul
            )
            (List.concat
                [ [ Html.li
                        [ Attributes.style "height" (String.fromFloat heightAbove ++ "px") ]
                        []
                  ]
                , List.indexedMap
                    (\index a ->
                        let
                            actualIndex =
                                index + dropped
                        in
                        viewEntry cfg
                            id
                            (selection == Just a)
                            (stuff.keyboardFocus == Just actualIndex)
                            (stuff.mouseFocus == Just actualIndex)
                            actualIndex
                            a
                    )
                    entries
                , [ Html.li
                        [ Attributes.style "height" (String.fromFloat heightBelow ++ "px") ]
                        []
                  ]
                ]
            )
        ]


handleKeydown :
    String
    -> Bool
    -> Bool
    -> Maybe Int
    -> List a
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
handleKeydown id jumpAtEnds open keyboardFocus entries entriesCount dropped code =
    let
        keyboardFocusedEntry =
            keyboardFocus
                |> Maybe.andThen
                    (\index -> elementAt (index - dropped) entries)
    in
    case code of
        " " ->
            if open then
                Decode.fail "not handling that key here"
            else
                Decode.succeed (TextfieldSpacePressed id)
                    |> preventDefault

        "ArrowUp" ->
            let
                previousIndex =
                    case keyboardFocus of
                        Nothing ->
                            entriesCount - 1

                        Just index ->
                            if jumpAtEnds && index == 0 then
                                entriesCount - 1
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index - 1)
            in
            scrollDataDecoder (previousIndex - dropped + 1)
                |> Decode.map (TextfieldArrowUpPressed id previousIndex)
                |> preventDefault

        "ArrowDown" ->
            let
                nextIndex =
                    case keyboardFocus of
                        Nothing ->
                            0

                        Just index ->
                            if jumpAtEnds && (index == entriesCount - 1) then
                                0
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index + 1)
            in
            scrollDataDecoder (nextIndex - dropped + 1)
                |> Decode.map (TextfieldArrowDownPressed id nextIndex)
                |> preventDefault

        "Enter" ->
            case keyboardFocusedEntry of
                Just entry ->
                    Decode.succeed (TextfieldEnterPressed entry)
                        |> allowDefault

                Nothing ->
                    Decode.fail "not handling that key here"

        "Escape" ->
            Decode.succeed TextfieldEscapePressed
                |> allowDefault

        _ ->
            Decode.fail "not handling that key here"


viewEntry :
    { cfg
        | closeAfterMouseSelection : Bool
        , li :
            { selected : Bool
            , keyboardFocused : Bool
            , mouseFocused : Bool
            }
            -> a
            -> HtmlDetails
        , entryId : a -> String
    }
    -> String
    -> Bool
    -> Bool
    -> Bool
    -> Int
    -> a
    -> Html (Msg a)
viewEntry cfg id selected keyboardFocused mouseFocused index a =
    let
        { attributes, children } =
            cfg.li
                { selected = selected
                , keyboardFocused = keyboardFocused
                , mouseFocused = mouseFocused
                }
                a
    in
    Html.li
        ([ Events.onMouseEnter (EntryMouseEntered index)
         , Events.onMouseLeave EntryMouseLeft
         , Events.onClick (EntryClicked id cfg.closeAfterMouseSelection index a)
         , Attributes.id (printEntryId id (cfg.entryId a))
         , Attributes.attribute "role" "option"
         ]
            |> appendAttributes attributes
        )
        (children
            |> List.map (Html.map (\_ -> NoOp))
        )



---- VIEW HELPER


setDisplay : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setDisplay open attrs =
    if open then
        attrs
    else
        Attributes.style "display" "none" :: attrs


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded open attrs =
    if open then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs


setAriaActivedescendant :
    (a -> String)
    -> Maybe Int
    -> List a
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant entryId keyboardFocus entries attrs =
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
                            (entryId a)
                            :: attrs
                    )
                |> Maybe.withDefault attrs


scrollDataDecoder : Int -> Decoder ScrollData
scrollDataDecoder index =
    Decode.succeed ScrollData
        |> Decode.requiredAt
            [ "target", "nextSibling", "scrollTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "nextSibling", "clientHeight" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "nextSibling", "childNodes", String.fromInt index, "offsetTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "nextSibling", "childNodes", String.fromInt index, "offsetHeight" ]
            Decode.float



-- IDS


printTextfieldId : String -> String
printTextfieldId id =
    id ++ "__textfield"


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



-- MISC


appendAttributes :
    List (Html.Attribute Never)
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
appendAttributes neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> NoOp))
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))


elementAt : Int -> List a -> Maybe a
elementAt index =
    List.drop index >> List.head



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- TEXTFIELD
    | TextfieldClicked String
    | TextfieldChanged String String
      --| TextfieldSpaceReleased
    | TextfieldSpacePressed String
    | TextfieldArrowUpPressed String Int ScrollData
    | TextfieldArrowDownPressed String Int ScrollData
    | TextfieldEnterPressed a
    | TextfieldEscapePressed
    | TextfieldBlured (Maybe ScrollData)
      -- LIST
    | ListMouseDown String
    | ListMouseUp
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered Int
    | EntryMouseLeft
    | EntryClicked String Bool Int a


{-| -}
update :
    { entrySelected : a -> msg
    , selectionDismissed : msg
    }
    -> Maybe a
    -> State a
    -> Msg a
    -> ( State a, Cmd (Msg a), Maybe msg )
update lifts selection ((State stuff) as state) msg =
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
            , resetScrollTop id stuff.keyboardFocus stuff.scrollDataCache
            , Nothing
            )

        TextfieldChanged id newQuery ->
            ( State
                { stuff
                    | open = True
                    , query = newQuery
                    , keyboardFocus = Nothing
                }
            , Browser.setScrollTop (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        TextfieldSpacePressed id ->
            ( State
                { stuff
                    | open = True
                    , scrollDataCache = Nothing
                }
            , resetScrollTop id stuff.keyboardFocus stuff.scrollDataCache
            , Nothing
            )

        TextfieldArrowUpPressed id previousIndex scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just previousIndex
                    , open = True
                    , ulScrollTop = scrollData.ulScrollTop
                    , ulClientHeight = scrollData.ulClientHeight
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        TextfieldArrowDownPressed id nextIndex scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just nextIndex
                    , open = True
                    , ulScrollTop = scrollData.ulScrollTop
                    , ulClientHeight = scrollData.ulClientHeight
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        TextfieldEnterPressed a ->
            ( State
                { stuff
                    | open = False
                    , keyboardFocus = Nothing
                }
            , Cmd.none
            , Just (lifts.entrySelected a)
            )

        TextfieldEscapePressed ->
            ( State
                { stuff
                    | open = False
                }
            , Cmd.none
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

        -- LIST
        ListMouseDown id ->
            ( State { stuff | preventBlur = True }
            , focusTextfield id
            , Nothing
            )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none, Nothing )

        ListScrolled ulScrollTop ulClientHeight ->
            ( State
                { stuff
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered index ->
            ( State { stuff | mouseFocus = Just index }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( State { stuff | mouseFocus = Nothing }
            , Cmd.none
            , Nothing
            )

        EntryClicked id closeAfterMouseSelection index a ->
            if closeAfterMouseSelection then
                ( State
                    { stuff
                        | open = False
                        , keyboardFocus = Nothing
                        , query = ""
                    }
                , focusTextfield id
                , Just (lifts.entrySelected a)
                )
            else
                ( State
                    { stuff
                        | keyboardFocus = Just index
                        , query = ""
                    }
                , focusTextfield id
                , Just (lifts.entrySelected a)
                )



-- CMDS


resetScrollTop : String -> Maybe Int -> Maybe ScrollData -> Cmd (Msg a)
resetScrollTop id keyboardFocus scrollDataCache =
    case scrollDataCache of
        Nothing ->
            Browser.setScrollTop (printListId id) 0
                |> Task.attempt (\_ -> NoOp)

        Just scrollData ->
            case keyboardFocus of
                Nothing ->
                    Browser.setScrollTop (printListId id) 0
                        |> Task.attempt (\_ -> NoOp)

                Just _ ->
                    adjustScrollTop id scrollData


adjustScrollTop : String -> ScrollData -> Cmd (Msg a)
adjustScrollTop id { ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } =
    if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
        Browser.setScrollTop (printListId id)
            (liOffsetTop + liOffsetHeight - ulClientHeight)
            |> Task.attempt (\_ -> NoOp)
    else if liOffsetTop < ulScrollTop then
        Browser.setScrollTop (printListId id) liOffsetTop
            |> Task.attempt (\_ -> NoOp)
    else
        Cmd.none


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)
