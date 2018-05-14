module DropdownMenu.Simple
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
        , keyboardFocus = Nothing
        , mouseFocus = Nothing
        , scrollDataCache = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 0
        }



---- CONFIG


{-| -}
type Config a
    = Config (ConfigData a)


type alias ConfigData a =
    { matchesQuery : String -> a -> Bool
    , entryId : a -> String

    -- BEHAVIOUR
    , jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool

    -- VIEW
    , container : HtmlAttributes
    , button :
        { selection : Maybe a
        , open : Bool
        }
        -> HtmlDetails
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
        }
    -> Maybe a
    -> State a
    -> List a
    -> Html (Msg a)
view cfg ids selection state entries =
    viewHelp cfg ids selection state <|
        { entries = entries
        , entriesCount = List.length entries
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
        }
    -> Maybe a
    -> State a
    -> List a
    -> Html (Msg a)
viewLazy entryHeight cfg ids selection ((State stuff) as state) entries =
    let
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
                entries
    in
    viewHelp cfg ids selection state <|
        { entries = List.reverse visibleEntries
        , entriesCount = List.length entries
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
        }
    -> Maybe a
    -> State a
    -> VisibleEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy } selection (State stuff) visibleEntries =
    let
        { entries, entriesCount, dropped, heightAbove, heightBelow } =
            visibleEntries

        { attributes, children } =
            cfg.button
                { selection = selection
                , open = stuff.open
                }
    in
    Html.div
        (appendAttributes cfg.container [])
        [ Html.button
            ([ Attributes.id (printTextfieldId id)
             , Attributes.attribute "aria-haspopup" "listbox"
             , Attributes.attribute "aria-labelledby"
                (printTextfieldId id ++ " " ++ labelledBy)
             , Attributes.style "position" "relative"
             , Attributes.tabindex 0
             , Events.onClick (ButtonClicked id)
             ]
                |> setAriaExpanded stuff.open
                |> appendAttributes attributes
            )
            (children
                |> List.map (Html.map (\_ -> NoOp))
            )
        , Html.ul
            ([ Attributes.id (printListId id)
             , Attributes.attribute "role" "listbox"
             , Attributes.attribute "aria-labelledby" labelledBy
             , Attributes.style "position" "absolute"
             , Attributes.tabindex -1
             , Events.on "mousedown" (Decode.succeed ListMouseDown)
             , Events.on "mouseup" (Decode.succeed ListMouseUp)
             , Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (handleKeydown id
                            cfg.jumpAtEnds
                            stuff.keyboardFocus
                            entries
                            entriesCount
                            dropped
                        )
                )
             , Events.on "blur" <|
                case stuff.keyboardFocus of
                    Nothing ->
                        Decode.succeed (ListBlured Nothing)

                    Just index ->
                        Decode.map (ListBlured << Just)
                            (scrollDataDecoder (index + 1 - dropped))
             , Events.on "scroll" <|
                Decode.map2 ListScrolled
                    (Decode.at [ "target", "scrollTop" ] Decode.float)
                    (Decode.at [ "target", "clientHeight" ] Decode.float)
             ]
                |> setDisplay stuff.open
                |> setAriaActivedescendant
                    (\a -> printEntryId id (cfg.entryId a))
                    (Maybe.map (\index -> index + 1 - dropped) stuff.keyboardFocus)
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
                        viewEntry cfg
                            id
                            (selection == Just a)
                            (stuff.keyboardFocus == Just (index + 1 - dropped))
                            (stuff.mouseFocus == Just (index + 1 - dropped))
                            dropped
                            index
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
    -> Maybe Int
    -> List a
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
handleKeydown id jumpAtEnds keyboardFocus entries entriesCount dropped code =
    let
        keyboardFocusedEntry =
            keyboardFocus
                |> Maybe.andThen
                    (\index -> elementAt (index - dropped) entries)
    in
    case code of
        "ArrowUp" ->
            let
                newIndex =
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
            scrollDataDecoder (newIndex + 1 - dropped)
                |> Decode.map (ListArrowUpPressed id newIndex)
                |> preventDefault

        "ArrowDown" ->
            let
                newIndex =
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
            scrollDataDecoder (newIndex + 1 - dropped)
                |> Decode.map (ListArrowDownPressed id newIndex)
                |> preventDefault

        "Enter" ->
            case keyboardFocusedEntry of
                Just entry ->
                    Decode.succeed (ListEnterPressed id entry)
                        |> allowDefault

                Nothing ->
                    Decode.fail "not handling that key here"

        "Escape" ->
            Decode.succeed (ListEscapePressed id)
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
    -> Int
    -> a
    -> Html (Msg a)
viewEntry cfg id selected keyboardFocused mouseFocused dropped index a =
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
        ([ Events.onMouseEnter (EntryMouseEntered (index + 1 - dropped))
         , Events.onMouseLeave EntryMouseLeft
         , Events.onClick (EntryClicked id cfg.closeAfterMouseSelection (index + 1 - dropped) a)
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
            [ "target", "scrollTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "clientHeight" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetHeight" ]
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
      -- BUTTON
    | ButtonClicked String
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowUpPressed String Int ScrollData
    | ListArrowDownPressed String Int ScrollData
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured (Maybe ScrollData)
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

        -- BUTTON
        ButtonClicked id ->
            ( State
                { stuff
                    | open = not stuff.open
                    , scrollDataCache = Nothing
                }
            , [ resetScrollTop id stuff.keyboardFocus stuff.scrollDataCache
              , focusList id
              ]
                |> Cmd.batch
            , Nothing
            )

        -- LIST
        ListMouseDown ->
            ( State { stuff | preventBlur = True }, Cmd.none, Nothing )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none, Nothing )

        ListArrowUpPressed id index scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just index
                    , open = True
                    , ulScrollTop = scrollData.ulScrollTop
                    , ulClientHeight = scrollData.ulClientHeight
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        ListArrowDownPressed id index scrollData ->
            ( State
                { stuff
                    | keyboardFocus = Just index
                    , open = True
                    , ulScrollTop = scrollData.ulScrollTop
                    , ulClientHeight = scrollData.ulClientHeight
                }
            , adjustScrollTop id scrollData
            , Nothing
            )

        ListEnterPressed id a ->
            ( State
                { stuff
                    | open = False
                    , keyboardFocus = Nothing
                }
            , focusButton id
            , Just (lifts.entrySelected a)
            )

        ListEscapePressed id ->
            ( State { stuff | open = False }
            , focusButton id
            , Nothing
            )

        ListBlured scrollDataCache ->
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
                    }
                , focusButton id
                , Just (lifts.entrySelected a)
                )
            else
                ( State { stuff | keyboardFocus = Just index }
                , Cmd.none
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


focusList : String -> Cmd (Msg a)
focusList id =
    Browser.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


focusButton : String -> Cmd (Msg a)
focusButton id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)
