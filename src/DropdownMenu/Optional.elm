module DropdownMenu.Optional
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

{-

   Copyright 2018 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

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
        , keyboardFocus : Maybe String
        , mouseFocus : Maybe String

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
    -> State a
    -> Maybe a
    -> List a
    -> Html (Msg a)
view cfg ids state selection entries =
    viewHelp cfg ids state selection entries <|
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
    -> State a
    -> Maybe a
    -> List a
    -> Html (Msg a)
viewLazy entryHeight cfg ids ((State stuff) as state) selection entries =
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
    viewHelp cfg ids state selection entries <|
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
    -> State a
    -> Maybe a
    -> List a
    -> VisibleEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy } (State stuff) selection allEntries visibleEntries =
    let
        { entries, entriesCount, dropped, heightAbove, heightBelow } =
            visibleEntries

        displayed =
            List.length entries

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
             , Events.on "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (buttonKeyDown id cfg.entryId cfg.jumpAtEnds stuff.keyboardFocus allEntries)
                )
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
                        (listKeydown id
                            cfg.jumpAtEnds
                            cfg.entryId
                            stuff.keyboardFocus
                            allEntries
                            dropped
                            displayed
                        )
                )
             , Events.on "blur" <|
                Decode.succeed ListBlured
             , Events.on "scroll" <|
                Decode.map2 ListScrolled
                    (Decode.at [ "target", "scrollTop" ] Decode.float)
                    (Decode.at [ "target", "clientHeight" ] Decode.float)
             ]
                |> setDisplay stuff.open
                |> setAriaActivedescendant
                    id
                    cfg.entryId
                    stuff.keyboardFocus
                    allEntries
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
                            (stuff.keyboardFocus == Just (cfg.entryId a))
                            (stuff.mouseFocus == Just (cfg.entryId a))
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


buttonKeyDown :
    String
    -> (a -> String)
    -> Bool
    -> Maybe String
    -> List a
    -> String
    -> Decoder (Msg a)
buttonKeyDown id entryId jumpAtEnds keyboardFocus allEntries code =
    case code of
        "ArrowUp" ->
            { arrowPressed = ButtonArrowPressed Up
            , newFocused =
                \menuId _ newEntryId ->
                    Decode.succeed <|
                        ButtonArrowPressed Up
                            UseScrollData
                            menuId
                            newEntryId
            }
                |> arrowUpPressed id entryId jumpAtEnds allEntries keyboardFocus

        "ArrowDown" ->
            { arrowPressed = ButtonArrowPressed Down
            , newFocused =
                \menuId _ newEntryId ->
                    Decode.succeed <|
                        ButtonArrowPressed Down
                            UseScrollData
                            menuId
                            newEntryId
            }
                |> arrowDownPressed id entryId jumpAtEnds allEntries keyboardFocus

        _ ->
            Decode.fail "not handling that key here"


listKeydown :
    String
    -> Bool
    -> (a -> String)
    -> Maybe String
    -> List a
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
listKeydown id jumpAtEnds entryId keyboardFocus allEntries dropped displayed code =
    case code of
        "ArrowUp" ->
            { arrowPressed = ListArrowPressed Up Nothing
            , newFocused =
                \menuId newIndex newEntryId ->
                    let
                        domIndex =
                            newIndex + 1 - dropped
                    in
                    if domIndex < 1 || domIndex > displayed then
                        Decode.succeed <|
                            ListArrowPressed Up Nothing UseScrollData menuId newEntryId
                    else
                        scrollDataDecoder (newIndex + 1 - dropped)
                            |> Decode.map
                                (\scrollData ->
                                    ListArrowPressed Up
                                        (Just scrollData)
                                        UseScrollData
                                        menuId
                                        newEntryId
                                )
            }
                |> arrowUpPressed id entryId jumpAtEnds allEntries keyboardFocus
                |> preventDefault

        "ArrowDown" ->
            { arrowPressed = ListArrowPressed Down Nothing
            , newFocused =
                \menuId newIndex newEntryId ->
                    let
                        domIndex =
                            newIndex + 1 - dropped
                    in
                    if domIndex < 1 || domIndex > displayed then
                        Decode.succeed <|
                            ListArrowPressed Down Nothing UseScrollData menuId newEntryId
                    else
                        scrollDataDecoder (newIndex + 1 - dropped)
                            |> Decode.map
                                (\scrollData ->
                                    ListArrowPressed Down
                                        (Just scrollData)
                                        UseScrollData
                                        menuId
                                        newEntryId
                                )
            }
                |> arrowDownPressed id entryId jumpAtEnds allEntries keyboardFocus
                |> preventDefault

        "Enter" ->
            keyboardFocus
                |> Maybe.andThen
                    (\currentFocus ->
                        find entryId currentFocus allEntries
                    )
                |> Maybe.map
                    (\( _, entry ) ->
                        Decode.succeed (ListEnterPressed id entry)
                            |> allowDefault
                    )
                |> Maybe.withDefault
                    (Decode.fail "not handling that key here")

        "Escape" ->
            Decode.succeed (ListEscapePressed id)
                |> allowDefault

        " " ->
            Decode.succeed NoOp
                |> preventDefault

        _ ->
            Decode.fail "not handling that key here"


arrowUpPressed :
    String
    -> (a -> String)
    -> Bool
    -> List a
    -> Maybe String
    ->
        { arrowPressed : ScrollAction -> String -> String -> msg
        , newFocused : String -> Int -> String -> Decoder msg
        }
    -> Decoder msg
arrowUpPressed id entryId jumpAtEnds allEntries focus msgs =
    case focus of
        Nothing ->
            case last allEntries of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just lastEntry ->
                    Decode.succeed <|
                        msgs.arrowPressed ScrollToEnd id (entryId lastEntry)

        Just currentFocus ->
            case findPrevious entryId currentFocus allEntries of
                Just (Last lastEntry) ->
                    if jumpAtEnds then
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToEnd id (entryId lastEntry)
                    else
                        Decode.succeed <|
                            msgs.arrowPressed UseScrollData id currentFocus

                Just (Previous newIndex newEntry) ->
                    msgs.newFocused id newIndex (entryId newEntry)

                Nothing ->
                    Decode.fail "not handling that key here"


arrowDownPressed :
    String
    -> (a -> String)
    -> Bool
    -> List a
    -> Maybe String
    ->
        { arrowPressed : ScrollAction -> String -> String -> msg
        , newFocused : String -> Int -> String -> Decoder msg
        }
    -> Decoder msg
arrowDownPressed id entryId jumpAtEnds allEntries focus msgs =
    case focus of
        Nothing ->
            case List.head allEntries of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just firstEntry ->
                    Decode.succeed <|
                        msgs.arrowPressed ScrollToEnd id (entryId firstEntry)

        Just currentFocus ->
            case findNext entryId currentFocus allEntries of
                Just (First firstEntry) ->
                    if jumpAtEnds then
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToEnd id (entryId firstEntry)
                    else
                        Decode.succeed <|
                            msgs.arrowPressed UseScrollData id currentFocus

                Just (Next newIndex newEntry) ->
                    msgs.newFocused id newIndex (entryId newEntry)

                Nothing ->
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
        ([ Events.onMouseEnter (EntryMouseEntered (cfg.entryId a))
         , Events.onMouseLeave EntryMouseLeft
         , Events.onClick (EntryClicked id cfg.closeAfterMouseSelection (cfg.entryId a) a)
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
setDisplay isOpen attrs =
    if isOpen then
        attrs
    else
        Attributes.style "display" "none" :: attrs


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded isOpen attrs =
    if isOpen then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs


setAriaActivedescendant :
    String
    -> (a -> String)
    -> Maybe String
    -> List a
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id entryId keyboardFocus entries attrs =
    case keyboardFocus of
        Nothing ->
            attrs

        Just focus ->
            entries
                |> find entryId focus
                |> Maybe.map
                    (\( _, focusedEntry ) ->
                        Attributes.attribute "aria-activedescendant"
                            (printEntryId id (entryId focusedEntry))
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



---- FIND CURRENT/NEXT/PREVIOUS ENTRIES


find : (a -> String) -> String -> List a -> Maybe ( Int, a )
find =
    findHelp 0


findHelp : Int -> (a -> String) -> String -> List a -> Maybe ( Int, a )
findHelp index entryId selectedId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if entryId entry == selectedId then
                Just ( index, entry )
            else
                findHelp (index + 1) entryId selectedId rest


type Previous a
    = Previous Int a
    | Last a


findPrevious : (a -> String) -> String -> List a -> Maybe (Previous a)
findPrevious entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if entryId first == currentId then
                last entries
                    |> Maybe.map Last
            else
                findPreviousHelp first 0 entryId currentId rest


findPreviousHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Previous a)
findPreviousHelp previous index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        next :: rest ->
            if entryId next == currentId then
                Just (Previous index previous)
            else
                findPreviousHelp next (index + 1) entryId currentId rest


type Next a
    = Next Int a
    | First a


findNext : (a -> String) -> String -> List a -> Maybe (Next a)
findNext entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            case rest of
                [] ->
                    if entryId first == currentId then
                        Just (First first)
                    else
                        Nothing

                next :: _ ->
                    if entryId first == currentId then
                        Just (Next 1 next)
                    else
                        findNextHelp first 1 entryId currentId rest


findNextHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Next a)
findNextHelp first index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            case rest of
                [] ->
                    Just (First first)

                next :: _ ->
                    if entryId entry == currentId then
                        Just (Next (index + 1) next)
                    else
                        findNextHelp first (index + 1) entryId currentId rest


last : List a -> Maybe a
last =
    List.reverse >> List.head



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- BUTTON
    | ButtonClicked String
    | ButtonArrowPressed Direction ScrollAction String String
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowPressed Direction (Maybe ScrollData) ScrollAction String String
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String Bool String a


type Direction
    = Up
    | Down


type ScrollAction
    = ScrollToEnd
    | UseScrollData


{-| -}
update : (a -> outMsg) -> State a -> Msg a -> ( State a, Cmd (Msg a), Maybe outMsg )
update entrySelected ((State stuff) as state) msg =
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

        ButtonArrowPressed direction scrollAction id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                    , ulScrollTop =
                        -- For some reason, we do not receive a scroll event
                        -- when we scroll the list to the top. We therefore set
                        -- `ulScrollTop` manually to `0` so the list is
                        -- displayed when using lazyView.
                        case scrollAction of
                            ScrollToEnd ->
                                case direction of
                                    Up ->
                                        stuff.ulScrollTop

                                    Down ->
                                        0

                            UseScrollData ->
                                stuff.ulScrollTop
                }
            , Cmd.batch
                [ focusList id
                , case scrollAction of
                    ScrollToEnd ->
                        case direction of
                            Up ->
                                Browser.setScrollBottom (printListId id) 0
                                    |> Task.attempt (\_ -> NoOp)

                            Down ->
                                Browser.setScrollTop (printListId id) 0
                                    |> Task.attempt (\_ -> NoOp)

                    UseScrollData ->
                        stuff.scrollDataCache
                            |> Maybe.map (centerScrollTop id)
                            |> Maybe.withDefault Cmd.none
                ]
            , Nothing
            )

        -- LIST
        ListMouseDown ->
            ( State { stuff | preventBlur = True }, Cmd.none, Nothing )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none, Nothing )

        ListArrowPressed direction maybeScrollData scrollAction id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
                |> (\((State newStuff) as newState) ->
                        case maybeScrollData of
                            Nothing ->
                                newState

                            Just scrollData ->
                                State
                                    { newStuff
                                        | ulScrollTop = scrollData.ulScrollTop
                                        , ulClientHeight = scrollData.ulClientHeight
                                        , scrollDataCache = Just scrollData
                                    }
                   )
            , case scrollAction of
                ScrollToEnd ->
                    case direction of
                        Up ->
                            Browser.setScrollBottom (printListId id) 0
                                |> Task.attempt (\_ -> NoOp)

                        Down ->
                            Browser.setScrollTop (printListId id) 0
                                |> Task.attempt (\_ -> NoOp)

                UseScrollData ->
                    case maybeScrollData of
                        Nothing ->
                            stuff.scrollDataCache
                                |> Maybe.map (centerScrollTop id)
                                |> Maybe.withDefault Cmd.none

                        Just scrollData ->
                            adjustScrollTop id scrollData
            , Nothing
            )

        ListEnterPressed id a ->
            ( State { stuff | open = False }
            , focusButton id
            , Just (entrySelected a)
            )

        ListEscapePressed id ->
            ( State { stuff | open = False }
            , focusButton id
            , Nothing
            )

        ListBlured ->
            ( State
                { stuff
                    | open =
                        if stuff.preventBlur then
                            stuff.open
                        else
                            False
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
        EntryMouseEntered newId ->
            ( State { stuff | mouseFocus = Just newId }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( State { stuff | mouseFocus = Nothing }
            , Cmd.none
            , Nothing
            )

        EntryClicked id closeAfterMouseSelection clickedId a ->
            ( if closeAfterMouseSelection then
                State { stuff | open = False }
              else
                State { stuff | keyboardFocus = Just clickedId }
            , if closeAfterMouseSelection then
                focusButton id
              else
                Cmd.none
            , Just (entrySelected a)
            )



-- CMDS


resetScrollTop : String -> Maybe String -> Maybe ScrollData -> Cmd (Msg a)
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
                    centerScrollTop id scrollData


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


centerScrollTop : String -> ScrollData -> Cmd (Msg a)
centerScrollTop id { ulClientHeight, liOffsetTop, liOffsetHeight } =
    Browser.setScrollTop (printListId id)
        (liOffsetTop + liOffsetHeight / 2 - ulClientHeight / 2)
        |> Task.attempt (\_ -> NoOp)


focusList : String -> Cmd (Msg a)
focusList id =
    Browser.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


focusButton : String -> Cmd (Msg a)
focusButton id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)
