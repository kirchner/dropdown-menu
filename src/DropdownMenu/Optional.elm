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
    -> List a
    -> Maybe a
    -> Html (Msg a)
view cfg ids state entries selection =
    viewHelp cfg ids state selection entries <|
        { spaceAboveFirst = 0
        , droppedAboveFirst = 0
        , spaceAboveSecond = 0
        , droppedAboveSecond = 0
        , spaceBelowFirst = 0
        , droppedBelowFirst = 0
        , spaceBelowSecond = 0
        , droppedBelowSecond = 0
        , entriesAbove = []
        , visibleEntries = entries
        , entriesBelow = []
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
    -> List a
    -> Maybe a
    -> Html (Msg a)
viewLazy entryHeight ((Config { entryId }) as cfg) ids ((State stuff) as state) entries selection =
    let
        maybeFocusIndex =
            stuff.keyboardFocus
                |> Maybe.andThen
                    (\focus ->
                        find entryId focus entries
                    )
                |> Maybe.map Tuple.first
    in
    computeRenderedEntries entryHeight
        stuff.ulScrollTop
        stuff.ulClientHeight
        maybeFocusIndex
        entries
        |> viewHelp cfg ids state selection entries


type alias RenderedEntries a =
    { spaceAboveFirst : Float
    , droppedAboveFirst : Int
    , spaceAboveSecond : Float
    , droppedAboveSecond : Int
    , spaceBelowFirst : Float
    , droppedBelowFirst : Int
    , spaceBelowSecond : Float
    , droppedBelowSecond : Int
    , entriesAbove : List a
    , visibleEntries : List a
    , entriesBelow : List a
    }


computeRenderedEntries : (a -> Float) -> Float -> Float -> Maybe Int -> List a -> RenderedEntries a
computeRenderedEntries entryHeight ulScrollTop ulClientHeight maybeFocusIndex entries =
    let
        initialRenderedEntries =
            { spaceAboveFirst = 0
            , droppedAboveFirst = 0
            , spaceAboveSecond = 0
            , droppedAboveSecond = 0
            , spaceBelowFirst = 0
            , droppedBelowFirst = 0
            , spaceBelowSecond = 0
            , droppedBelowSecond = 0
            , entriesAbove = []
            , visibleEntries = []
            , entriesBelow = []
            }

        withoutIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                { data
                    | spaceAboveFirst = data.spaceAboveFirst + height
                    , droppedAboveFirst = data.droppedAboveFirst + 1
                }
            else if currentHeight >= (ulScrollTop + ulClientHeight + 200) then
                { data
                    | spaceBelowFirst = data.spaceBelowFirst + height
                    , droppedBelowFirst = data.droppedBelowFirst + 1
                }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        withIndex index currentIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceAboveFirst = data.spaceAboveFirst + height
                        , droppedAboveFirst = data.droppedAboveFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceAboveSecond = data.spaceAboveSecond + height
                        , droppedAboveSecond = data.droppedAboveSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesAbove = entry :: data.entriesAbove }
            else if currentHeight > (ulScrollTop + ulClientHeight + 200) then
                -- entry is below the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceBelowFirst = data.spaceBelowFirst + height
                        , droppedBelowFirst = data.droppedBelowFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceBelowSecond = data.spaceBelowSecond + height
                        , droppedBelowSecond = data.droppedBelowSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesBelow = entry :: data.entriesBelow }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        reverseLists renderedEntries =
            { renderedEntries
                | entriesAbove = List.reverse renderedEntries.entriesAbove
                , visibleEntries = List.reverse renderedEntries.visibleEntries
                , entriesBelow = List.reverse renderedEntries.entriesBelow
            }
    in
    reverseLists <|
        case maybeFocusIndex of
            Nothing ->
                entries
                    |> List.foldl
                        (\entry ( currentHeight, data ) ->
                            ( currentHeight + entryHeight entry
                            , withoutIndex entry currentHeight data
                            )
                        )
                        ( 0, initialRenderedEntries )
                    |> Tuple.second

            Just index ->
                entries
                    |> List.foldl
                        (\entry ( ( currentIndex, currentHeight ), data ) ->
                            ( ( currentIndex + 1
                              , currentHeight + entryHeight entry
                              )
                            , withIndex index currentIndex entry currentHeight data
                            )
                        )
                        ( ( 0, 0 ), initialRenderedEntries )
                    |> Tuple.second


viewHelp :
    Config a
    ->
        { id : String
        , labelledBy : String
        }
    -> State a
    -> Maybe a
    -> List a
    -> RenderedEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy } (State stuff) selection allEntries renderedEntries =
    let
        displayed =
            List.length renderedEntries.visibleEntries

        dropped =
            renderedEntries.droppedAboveFirst + renderedEntries.droppedAboveSecond

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
                            renderedEntries.droppedAboveFirst
                            renderedEntries.droppedAboveSecond
                            renderedEntries.droppedBelowFirst
                            displayed
                        )
                )
             , Events.on "blur" <|
                Decode.succeed ListBlured
             , Events.on "scroll" <|
                Decode.map3 ListScrolled
                    (Decode.at [ "target", "scrollTop" ] Decode.float)
                    (Decode.at [ "target", "clientHeight" ] Decode.float)
                    (Decode.at [ "target", "scrollHeight" ] Decode.float)
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
                [ spacer renderedEntries.spaceAboveFirst
                , renderedEntries.entriesAbove
                    |> List.indexedMap
                        (\index a ->
                            let
                                actualIndex =
                                    index + renderedEntries.droppedAboveFirst
                            in
                            viewEntry cfg
                                id
                                (selection == Just a)
                                (stuff.keyboardFocus == Just (cfg.entryId a))
                                (stuff.mouseFocus == Just (cfg.entryId a))
                                actualIndex
                                a
                        )
                , spacer renderedEntries.spaceAboveSecond
                , renderedEntries.visibleEntries
                    |> List.indexedMap
                        (\index a ->
                            let
                                actualIndex =
                                    index
                                        + renderedEntries.droppedAboveFirst
                                        + renderedEntries.droppedAboveSecond
                            in
                            viewEntry cfg
                                id
                                (selection == Just a)
                                (stuff.keyboardFocus == Just (cfg.entryId a))
                                (stuff.mouseFocus == Just (cfg.entryId a))
                                actualIndex
                                a
                        )
                , spacer renderedEntries.spaceBelowFirst
                , renderedEntries.entriesBelow
                    |> List.indexedMap
                        (\index a ->
                            let
                                actualIndex =
                                    index
                                        + renderedEntries.droppedAboveFirst
                                        + renderedEntries.droppedAboveSecond
                                        + renderedEntries.droppedBelowFirst
                                        + displayed
                            in
                            viewEntry cfg
                                id
                                (selection == Just a)
                                (stuff.keyboardFocus == Just (cfg.entryId a))
                                (stuff.mouseFocus == Just (cfg.entryId a))
                                actualIndex
                                a
                        )
                , spacer renderedEntries.spaceBelowSecond
                ]
            )
        ]


spacer : Float -> List (Html msg)
spacer height =
    [ Html.li
        (if height == 0 then
            [ Attributes.style "display" "none" ]
         else
            [ Attributes.style "height" (String.fromFloat height ++ "px") ]
        )
        []
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
            { arrowPressed = ButtonArrowPressed
            , newFocused =
                \menuId _ newEntryId ->
                    Decode.succeed <|
                        ButtonArrowPressed UseScrollData menuId newEntryId
            }
                |> arrowUpPressed id entryId jumpAtEnds allEntries keyboardFocus

        "ArrowDown" ->
            { arrowPressed = ButtonArrowPressed
            , newFocused =
                \menuId _ newEntryId ->
                    Decode.succeed <|
                        ButtonArrowPressed UseScrollData menuId newEntryId
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
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
listKeydown id jumpAtEnds entryId keyboardFocus allEntries droppedAboveFirst droppedAboveSecond droppedBelowFirst displayed code =
    case code of
        "ArrowUp" ->
            { arrowPressed = ListArrowPressed Nothing
            , newFocused =
                \menuId newIndex newEntryId ->
                    let
                        domIndex =
                            if newIndex - droppedAboveFirst < 0 then
                                -- newEntry is not rendered, should never
                                -- happen
                                0
                            else if newIndex - droppedAboveFirst < 3 then
                                -- newEntry is above visible entries
                                newIndex - droppedAboveFirst + 1
                            else if
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    < displayed
                            then
                                -- newEntry is visible
                                newIndex - droppedAboveFirst - droppedAboveSecond + 2
                            else if
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    - droppedBelowFirst
                                    - displayed
                                    < 3
                            then
                                -- newEntry is below visible entries
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    - droppedBelowFirst
                                    + 3
                            else
                                0
                    in
                    scrollDataDecoder domIndex
                        |> Decode.map
                            (\scrollData ->
                                ListArrowPressed (Just scrollData)
                                    UseScrollData
                                    menuId
                                    newEntryId
                            )
            }
                |> arrowUpPressed id entryId jumpAtEnds allEntries keyboardFocus
                |> preventDefault

        "ArrowDown" ->
            { arrowPressed = ListArrowPressed Nothing
            , newFocused =
                \menuId newIndex newEntryId ->
                    let
                        domIndex =
                            if newIndex - droppedAboveFirst < 0 then
                                -- newEntry is not rendered, should never
                                -- happen
                                0
                            else if newIndex - droppedAboveFirst < 3 then
                                -- newEntry is above visible entries
                                newIndex - droppedAboveFirst + 1
                            else if
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    < displayed
                            then
                                -- newEntry is visible
                                newIndex - droppedAboveFirst - droppedAboveSecond + 2
                            else if
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    - droppedBelowFirst
                                    - displayed
                                    < 3
                            then
                                -- newEntry is below visible entries
                                newIndex
                                    - droppedAboveFirst
                                    - droppedAboveSecond
                                    - droppedBelowFirst
                                    + 3
                            else
                                0
                    in
                    scrollDataDecoder domIndex
                        |> Decode.map
                            (\scrollData ->
                                ListArrowPressed (Just scrollData)
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
    let
        focusLastEntry =
            case last allEntries of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just lastEntry ->
                    Decode.succeed <|
                        msgs.arrowPressed ScrollToBottom id (entryId lastEntry)
    in
    case focus of
        Nothing ->
            focusLastEntry

        Just currentFocus ->
            case findPrevious entryId currentFocus allEntries of
                Just (Last lastEntry) ->
                    if jumpAtEnds then
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToBottom id (entryId lastEntry)
                    else
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToTop id currentFocus

                Just (Previous newIndex newEntry) ->
                    msgs.newFocused id newIndex (entryId newEntry)

                Nothing ->
                    focusLastEntry


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
    let
        focusFirstEntry =
            case List.head allEntries of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just firstEntry ->
                    Decode.succeed <|
                        msgs.arrowPressed ScrollToTop id (entryId firstEntry)
    in
    case focus of
        Nothing ->
            focusFirstEntry

        Just currentFocus ->
            case findNext entryId currentFocus allEntries of
                Just (First firstEntry) ->
                    if jumpAtEnds then
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToTop id (entryId firstEntry)
                    else
                        Decode.succeed <|
                            msgs.arrowPressed ScrollToBottom id currentFocus

                Just (Next newIndex newEntry) ->
                    msgs.newFocused id newIndex (entryId newEntry)

                Nothing ->
                    focusFirstEntry


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
    | ButtonArrowPressed ScrollAction String String
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowPressed (Maybe ScrollData) ScrollAction String String
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured
    | ListScrolled Float Float Float
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String Bool String a


type Direction
    = Up
    | Down


type ScrollAction
    = ScrollToTop
    | ScrollToBottom
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

        ButtonArrowPressed scrollAction id newFocus ->
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
                            ScrollToTop ->
                                0

                            ScrollToBottom ->
                                stuff.ulScrollTop

                            UseScrollData ->
                                stuff.ulScrollTop
                }
            , Cmd.batch
                [ focusList id
                , case scrollAction of
                    ScrollToTop ->
                        Browser.setScrollTop (printListId id) 0
                            |> Task.attempt (\_ -> NoOp)

                    ScrollToBottom ->
                        Browser.setScrollBottom (printListId id) 0
                            |> Task.attempt (\_ -> NoOp)

                    UseScrollData ->
                        stuff.scrollDataCache
                            |> Maybe.map (centerScrollTop id)
                            |> Maybe.withDefault
                                (Browser.scrollIntoView (printEntryId id newFocus)
                                    |> Task.attempt (\_ -> NoOp)
                                )
                ]
            , Nothing
            )

        -- LIST
        ListMouseDown ->
            ( State { stuff | preventBlur = True }, Cmd.none, Nothing )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none, Nothing )

        ListArrowPressed maybeScrollData scrollAction id newFocus ->
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
                ScrollToTop ->
                    Browser.setScrollTop (printListId id) 0
                        |> Task.attempt (\_ -> NoOp)

                ScrollToBottom ->
                    Browser.setScrollBottom (printListId id) 0
                        |> Task.attempt (\_ -> NoOp)

                UseScrollData ->
                    case maybeScrollData of
                        Nothing ->
                            stuff.scrollDataCache
                                |> Maybe.map (centerScrollTop id)
                                |> Maybe.withDefault
                                    (Browser.scrollIntoView (printEntryId id newFocus)
                                        |> Task.attempt (\_ -> NoOp)
                                    )

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

        ListScrolled ulScrollTop ulClientHeight ulScrollHeight ->
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
adjustScrollTop id ({ ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } as scrollData) =
    if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
        if liOffsetTop <= ulScrollTop + ulClientHeight then
            Browser.setScrollTop (printListId id)
                (liOffsetTop + liOffsetHeight - ulClientHeight)
                |> Task.attempt (\_ -> NoOp)
        else
            centerScrollTop id scrollData
    else if liOffsetTop < ulScrollTop then
        if liOffsetTop + liOffsetHeight >= ulScrollTop then
            Browser.setScrollTop (printListId id) liOffsetTop
                |> Task.attempt (\_ -> NoOp)
        else
            centerScrollTop id scrollData
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
