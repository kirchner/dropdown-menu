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
        { before : List a
        , selection : Maybe a
        , after : List a

        -- UI
        , open : Bool
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
closed : List a -> Maybe a -> List a -> State a
closed initialBefore initialSelection initialAfter =
    State
        { before = initialBefore
        , selection = initialSelection
        , after = initialAfter
        , open = False
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



---- READING


{-| -}
toList : State a -> List a
toList (State stuff) =
    case stuff.selection of
        Nothing ->
            stuff.before ++ stuff.after

        Just current ->
            stuff.before ++ current :: stuff.after


{-| -}
before : State a -> List a
before (State stuff) =
    stuff.before


{-| -}
selection : State a -> Maybe a
selection (State stuff) =
    stuff.selection


{-| -}
after : State a -> List a
after (State stuff) =
    stuff.after



--{-| -}
--hovered : State a -> Maybe a
--hovered ((State { mouseFocus }) as state) =
--    mouseFocus
--        |> Maybe.andThen
--            (\index ->
--                state
--                    |> toList
--                    |> elementAt index
--            )
--
--
--{-| -}
--focused : State a -> Maybe a
--focused ((State { keyboardFocus }) as state) =
--    keyboardFocus
--        |> Maybe.andThen
--            (\index ->
--                state
--                    |> toList
--                    |> elementAt index
--            )


open : State a -> Bool
open (State stuff) =
    stuff.open



---- TRANSFORMING


{-| -}
map : (a -> b) -> State a -> State b
map func (State stuff) =
    State
        { before = List.map func stuff.before
        , selection = Maybe.map func stuff.selection
        , after = List.map func stuff.after

        -- UI
        , open = stuff.open
        , preventBlur = stuff.preventBlur

        -- FOCUS
        , keyboardFocus = stuff.keyboardFocus
        , mouseFocus = stuff.mouseFocus

        -- DOM MEASUREMENTS
        , scrollDataCache = stuff.scrollDataCache
        , ulScrollTop = stuff.ulScrollTop
        , ulClientHeight = stuff.ulClientHeight
        }


{-| Select the first element which passes the provided predicate function. If
no element matches, the current selection will be dismissed.

**Note:** This will also close the dropdown menu.

-}
select : (a -> Bool) -> State a -> State a
select isTheOne state =
    selectHelp isTheOne state
        |> (\(State stuff) ->
                State
                    { stuff
                        | open = False
                        , scrollDataCache = Nothing
                    }
           )


selectHelp : (a -> Bool) -> State a -> State a
selectHelp isTheOne ((State stuff) as state) =
    let
        ( newBefore, newSelection, newAfter ) =
            state
                |> toList
                |> List.foldl
                    (\a ( tmpBefore, tmpSelection, tmpAfter ) ->
                        case tmpSelection of
                            Nothing ->
                                if isTheOne a then
                                    ( tmpBefore
                                    , Just a
                                    , tmpAfter
                                    )
                                else
                                    ( a :: tmpBefore
                                    , Nothing
                                    , tmpAfter
                                    )

                            Just _ ->
                                ( tmpBefore
                                , tmpSelection
                                , a :: tmpAfter
                                )
                    )
                    ( [], Nothing, [] )
    in
    State
        { stuff
            | before = List.reverse newBefore
            , selection = newSelection
            , after = List.reverse newAfter
            , keyboardFocus = Nothing
            , mouseFocus = Nothing
        }


{-| -}
append : List a -> State a -> State a
append list (State stuff) =
    State { stuff | after = List.append list stuff.after }


{-| -}
prepend : List a -> State a -> State a
prepend list (State stuff) =
    State
        { stuff
            | before = List.append stuff.before list
            , open = False
            , keyboardFocus = Nothing
            , mouseFocus = Nothing
            , scrollDataCache = Nothing
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
    -> Html (Msg a)
view cfg ids state =
    let
        entries =
            toList state
    in
    viewHelp cfg ids state entries <|
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
    -> Html (Msg a)
viewLazy entryHeight cfg ids ((State stuff) as state) =
    let
        entries =
            case stuff.selection of
                Nothing ->
                    stuff.before ++ stuff.after

                Just current ->
                    stuff.before ++ current :: stuff.after

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
    viewHelp cfg ids state entries <|
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
    -> List a
    -> VisibleEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy } (State stuff) allEntries visibleEntries =
    let
        { entries, entriesCount, dropped, heightAbove, heightBelow } =
            visibleEntries

        displayed =
            List.length entries

        { attributes, children } =
            cfg.button
                { selection = stuff.selection
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
                        (\code ->
                            case code of
                                "ArrowUp" ->
                                    case stuff.keyboardFocus of
                                        Nothing ->
                                            case last allEntries of
                                                Nothing ->
                                                    Decode.fail "not handling that key here"

                                                Just lastEntry ->
                                                    Decode.succeed
                                                        (ButtonArrowUpPressedWithoutFocus id
                                                            (cfg.entryId lastEntry)
                                                        )

                                        Just currentFocus ->
                                            case findPrevious cfg.entryId currentFocus allEntries of
                                                Just (Last lastEntry) ->
                                                    if cfg.jumpAtEnds then
                                                        Decode.succeed
                                                            (ButtonArrowUpPressedWrapping id
                                                                (cfg.entryId lastEntry)
                                                            )
                                                    else
                                                        Decode.succeed
                                                            (ButtonArrowDownPressed id currentFocus)

                                                Just (Previous newIndex newEntry) ->
                                                    Decode.succeed
                                                        (ButtonArrowUpPressed id
                                                            (cfg.entryId newEntry)
                                                        )

                                                Nothing ->
                                                    Decode.fail "not handling that key here"

                                "ArrowDown" ->
                                    case stuff.keyboardFocus of
                                        Nothing ->
                                            case List.head allEntries of
                                                Nothing ->
                                                    Decode.fail "not handling that key here"

                                                Just firstEntry ->
                                                    Decode.succeed
                                                        (ButtonArrowDownPressedWithoutFocus id
                                                            (cfg.entryId firstEntry)
                                                        )

                                        Just currentFocus ->
                                            case findNext cfg.entryId currentFocus allEntries of
                                                Just (First firstEntry) ->
                                                    if cfg.jumpAtEnds then
                                                        Decode.succeed
                                                            (ButtonArrowDownPressedWrapping id
                                                                (cfg.entryId firstEntry)
                                                            )
                                                    else
                                                        Decode.succeed
                                                            (ButtonArrowDownPressed id currentFocus)

                                                Just (Next newIndex newEntry) ->
                                                    Decode.succeed
                                                        (ButtonArrowDownPressed id
                                                            (cfg.entryId newEntry)
                                                        )

                                                Nothing ->
                                                    Decode.fail "not handling that key here"

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
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
                        (handleKeydown id
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
                            (stuff.selection == Just a)
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


handleKeydown :
    String
    -> Bool
    -> (a -> String)
    -> Maybe String
    -> List a
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
handleKeydown id jumpAtEnds entryId keyboardFocus allEntries dropped displayed code =
    case code of
        "ArrowUp" ->
            case keyboardFocus of
                Nothing ->
                    case last allEntries of
                        Nothing ->
                            Decode.fail "not handling that key here"

                        Just lastEntry ->
                            Decode.succeed (ListArrowUpPressedWihoutFocus id (entryId lastEntry))
                                |> preventDefault

                Just currentFocus ->
                    case findPrevious entryId currentFocus allEntries of
                        Just (Last lastEntry) ->
                            if jumpAtEnds then
                                Decode.succeed (ListArrowUpPressedWrapping id (entryId lastEntry))
                                    |> preventDefault
                            else
                                Decode.succeed (ListArrowUpPressed id currentFocus Nothing)
                                    |> preventDefault

                        Just (Previous newIndex newEntry) ->
                            let
                                domIndex =
                                    newIndex + 1 - dropped
                            in
                            if domIndex < 1 || domIndex > displayed then
                                Decode.succeed (ListArrowUpPressed id (entryId newEntry) Nothing)
                                    |> preventDefault
                            else
                                scrollDataDecoder (newIndex + 1 - dropped)
                                    |> Decode.map (ListArrowUpPressed id (entryId newEntry) << Just)
                                    |> preventDefault

                        Nothing ->
                            Decode.fail "not handling that key here"

        "ArrowDown" ->
            case keyboardFocus of
                Nothing ->
                    case List.head allEntries of
                        Nothing ->
                            Decode.fail "not handling that key here"

                        Just firstEntry ->
                            Decode.succeed (ListArrowDownPressedWihoutFocus id (entryId firstEntry))
                                |> preventDefault

                Just currentFocus ->
                    case findNext entryId currentFocus allEntries of
                        Just (First firstEntry) ->
                            if jumpAtEnds then
                                Decode.succeed (ListArrowDownPressedWrapping id (entryId firstEntry))
                                    |> preventDefault
                            else
                                Decode.succeed (ListArrowUpPressed id currentFocus Nothing)
                                    |> preventDefault

                        Just (Next newIndex newEntry) ->
                            let
                                domIndex =
                                    newIndex + 1 - dropped
                            in
                            if domIndex < 1 || domIndex > displayed then
                                Decode.succeed (ListArrowDownPressed id (entryId newEntry) Nothing)
                                    |> preventDefault
                            else
                                scrollDataDecoder (newIndex + 1 - dropped)
                                    |> Decode.map (ListArrowDownPressed id (entryId newEntry) << Just)
                                    |> preventDefault

                        Nothing ->
                            Decode.fail "not handling that key here"

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



--


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



--


findWithNext : (a -> String) -> String -> List a -> Maybe ( Int, a, a )
findWithNext entryId selectedId entries =
    case entries of
        [] ->
            Nothing

        first :: [] ->
            if entryId first == selectedId then
                Just ( 0, first, first )
            else
                Nothing

        first :: rest ->
            findWithNextHelp first 1 entryId selectedId rest


findWithNextHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe ( Int, a, a )
findWithNextHelp first index entryId selectedId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            case rest of
                [] ->
                    if entryId entry == selectedId then
                        Just ( index, entry, first )
                    else
                        Nothing

                next :: _ ->
                    if entryId entry == selectedId then
                        Just ( index, entry, next )
                    else
                        findWithNextHelp first (index + 1) entryId selectedId rest



--


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



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- BUTTON
    | ButtonClicked String
    | ButtonArrowUpPressed String String
    | ButtonArrowDownPressed String String
    | ButtonArrowUpPressedWithoutFocus String String
    | ButtonArrowDownPressedWithoutFocus String String
    | ButtonArrowUpPressedWrapping String String
    | ButtonArrowDownPressedWrapping String String
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowUpPressed String String (Maybe ScrollData)
    | ListArrowDownPressed String String (Maybe ScrollData)
    | ListArrowUpPressedWihoutFocus String String
    | ListArrowDownPressedWihoutFocus String String
    | ListArrowUpPressedWrapping String String
    | ListArrowDownPressedWrapping String String
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String Bool String a


{-| -}
update : State a -> Msg a -> ( State a, Cmd (Msg a) )
update ((State stuff) as state) msg =
    case msg of
        NoOp ->
            ( state, Cmd.none )

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
            )

        ButtonArrowUpPressed id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ stuff.scrollDataCache
                    |> Maybe.map (centerScrollTop id)
                    |> Maybe.withDefault Cmd.none
                , focusList id
                ]
            )

        ButtonArrowDownPressed id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ stuff.scrollDataCache
                    |> Maybe.map (centerScrollTop id)
                    |> Maybe.withDefault Cmd.none
                , focusList id
                ]
            )

        ButtonArrowUpPressedWithoutFocus id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ Browser.setScrollBottom (printListId id) 0
                    |> Task.attempt (\_ -> NoOp)
                , focusList id
                ]
            )

        ButtonArrowDownPressedWithoutFocus id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ Browser.setScrollTop (printListId id) 0
                    |> Task.attempt (\_ -> NoOp)
                , focusList id
                ]
            )

        ButtonArrowUpPressedWrapping id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ Browser.setScrollBottom (printListId id) 0
                    |> Task.attempt (\_ -> NoOp)
                , focusList id
                ]
            )

        ButtonArrowDownPressedWrapping id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Cmd.batch
                [ Browser.setScrollTop (printListId id) 0
                    |> Task.attempt (\_ -> NoOp)
                , focusList id
                ]
            )

        -- LIST
        ListMouseDown ->
            ( State { stuff | preventBlur = True }, Cmd.none )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none )

        ListArrowUpPressed id newFocus maybeScrollData ->
            case maybeScrollData of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just newFocus
                            , open = True
                        }
                    , stuff.scrollDataCache
                        |> Maybe.map (centerScrollTop id)
                        |> Maybe.withDefault Cmd.none
                    )

                Just scrollData ->
                    ( State
                        { stuff
                            | keyboardFocus = Just newFocus
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , adjustScrollTop id scrollData
                    )

        ListArrowDownPressed id newFocus maybeScrollData ->
            case maybeScrollData of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just newFocus
                            , open = True
                        }
                    , stuff.scrollDataCache
                        |> Maybe.map (centerScrollTop id)
                        |> Maybe.withDefault Cmd.none
                    )

                Just scrollData ->
                    ( State
                        { stuff
                            | keyboardFocus = Just newFocus
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , adjustScrollTop id scrollData
                    )

        ListArrowUpPressedWihoutFocus id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Browser.setScrollBottom (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            )

        ListArrowDownPressedWihoutFocus id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Browser.setScrollTop (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            )

        ListArrowUpPressedWrapping id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Browser.setScrollBottom (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            )

        ListArrowDownPressedWrapping id newFocus ->
            ( State
                { stuff
                    | keyboardFocus = Just newFocus
                    , open = True
                }
            , Browser.setScrollTop (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            )

        ListEnterPressed id a ->
            ( state
                |> selectHelp ((==) a)
                |> (\(State newStuff) ->
                        State
                            { newStuff | open = False }
                   )
            , focusButton id
            )

        ListEscapePressed id ->
            ( State { stuff | open = False }
            , focusButton id
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
            )

        ListScrolled ulScrollTop ulClientHeight ->
            ( State
                { stuff
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            )

        -- ENTRY
        EntryMouseEntered newId ->
            ( State { stuff | mouseFocus = Just newId }
            , Cmd.none
            )

        EntryMouseLeft ->
            ( State { stuff | mouseFocus = Nothing }
            , Cmd.none
            )

        EntryClicked id closeAfterMouseSelection clickedId a ->
            ( state
                |> selectHelp ((==) a)
                |> (\(State newStuff) ->
                        if closeAfterMouseSelection then
                            State { newStuff | open = False }
                        else
                            State { newStuff | keyboardFocus = Just clickedId }
                   )
            , if closeAfterMouseSelection then
                focusButton id
              else
                Cmd.none
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
