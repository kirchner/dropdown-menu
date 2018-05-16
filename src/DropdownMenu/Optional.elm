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


{-| -}
hovered : State a -> Maybe a
hovered ((State { mouseFocus }) as state) =
    mouseFocus
        |> Maybe.andThen
            (\index ->
                state
                    |> toList
                    |> elementAt index
            )


{-| -}
focused : State a -> Maybe a
focused ((State { keyboardFocus }) as state) =
    keyboardFocus
        |> Maybe.andThen
            (\index ->
                state
                    |> toList
                    |> elementAt index
            )


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
    viewHelp cfg ids state <|
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
    viewHelp cfg ids state <|
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
    -> VisibleEntries a
    -> Html (Msg a)
viewHelp (Config cfg) { id, labelledBy } (State stuff) visibleEntries =
    let
        { entries, entriesCount, dropped, heightAbove, heightBelow } =
            visibleEntries

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
                                    Decode.succeed (ButtonArrowUpPressed id cfg.jumpAtEnds)

                                "ArrowDown" ->
                                    Decode.succeed (ButtonArrowDownPressed id cfg.jumpAtEnds)

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
                            stuff.keyboardFocus
                            entries
                            entriesCount
                            dropped
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
                            (stuff.selection == Just a)
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
    -> Maybe Int
    -> List a
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
handleKeydown id jumpAtEnds keyboardFocus visibleEntries entriesCount dropped code =
    let
        keyboardFocusedEntry =
            keyboardFocus
                |> Maybe.andThen
                    (\index -> elementAt (index - dropped) visibleEntries)
    in
    case code of
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
                |> Decode.map (ListArrowUpPressed id jumpAtEnds)
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
                |> Decode.map (ListArrowDownPressed id jumpAtEnds)
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
    | ButtonArrowUpPressed String Bool
    | ButtonArrowDownPressed String Bool
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowUpPressed String Bool ScrollData
    | ListArrowDownPressed String Bool ScrollData
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered Int
    | EntryMouseLeft
    | EntryClicked String Bool Int a


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

        ButtonArrowUpPressed id jumpAtEnds ->
            let
                entriesCount =
                    state
                        |> toList
                        |> List.length
            in
            case stuff.keyboardFocus of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just (entriesCount - 1)
                            , open = True
                        }
                    , Cmd.batch
                        [ Browser.setScrollBottom (printListId id) 0
                            |> Task.attempt (\_ -> NoOp)
                        , focusList id
                        ]
                    )

                Just index ->
                    let
                        newIndex =
                            if jumpAtEnds && index == 0 then
                                entriesCount - 1
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index - 1)
                    in
                    ( State
                        { stuff
                            | keyboardFocus = Just newIndex
                            , open = True
                        }
                    , Cmd.batch
                        [ focusList id
                        , stuff.scrollDataCache
                            |> Maybe.map (centerScrollTop id)
                            |> Maybe.withDefault Cmd.none
                        ]
                    )

        ButtonArrowDownPressed id jumpAtEnds ->
            let
                entriesCount =
                    state
                        |> toList
                        |> List.length
            in
            case stuff.keyboardFocus of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just 0
                            , open = True
                        }
                    , Cmd.batch
                        [ Browser.setScrollTop (printListId id) 0
                            |> Task.attempt (\_ -> NoOp)
                        , focusList id
                        ]
                    )

                Just index ->
                    let
                        newIndex =
                            if jumpAtEnds && index == entriesCount - 1 then
                                0
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index + 1)
                    in
                    ( State
                        { stuff
                            | keyboardFocus = Just newIndex
                            , open = True
                        }
                    , Cmd.batch
                        [ focusList id
                        , stuff.scrollDataCache
                            |> Maybe.map (centerScrollTop id)
                            |> Maybe.withDefault Cmd.none
                        ]
                    )

        -- LIST
        ListMouseDown ->
            ( State { stuff | preventBlur = True }, Cmd.none )

        ListMouseUp ->
            ( State { stuff | preventBlur = False }, Cmd.none )

        ListArrowUpPressed id jumpAtEnds scrollData ->
            let
                entriesCount =
                    state
                        |> toList
                        |> List.length
            in
            case stuff.keyboardFocus of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just (entriesCount - 1)
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , Browser.setScrollBottom (printListId id) 0
                        |> Task.attempt (\_ -> NoOp)
                    )

                Just index ->
                    let
                        newIndex =
                            if jumpAtEnds && index == 0 then
                                entriesCount - 1
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index - 1)
                    in
                    ( State
                        { stuff
                            | keyboardFocus = Just newIndex
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , adjustScrollTop id scrollData
                    )

        ListArrowDownPressed id jumpAtEnds scrollData ->
            let
                entriesCount =
                    state
                        |> toList
                        |> List.length
            in
            case stuff.keyboardFocus of
                Nothing ->
                    ( State
                        { stuff
                            | keyboardFocus = Just 0
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , Browser.setScrollTop (printListId id) 0
                        |> Task.attempt (\_ -> NoOp)
                    )

                Just index ->
                    let
                        newIndex =
                            if jumpAtEnds && index == entriesCount - 1 then
                                0
                            else
                                clamp 0 (entriesCount - 1) <|
                                    (index + 1)
                    in
                    ( State
                        { stuff
                            | keyboardFocus = Just newIndex
                            , open = True
                            , ulScrollTop = scrollData.ulScrollTop
                            , ulClientHeight = scrollData.ulClientHeight
                            , scrollDataCache = Just scrollData
                        }
                    , adjustScrollTop id scrollData
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
        EntryMouseEntered index ->
            ( State { stuff | mouseFocus = Just index }
            , Cmd.none
            )

        EntryMouseLeft ->
            ( State { stuff | mouseFocus = Nothing }
            , Cmd.none
            )

        EntryClicked id closeAfterMouseSelection index a ->
            ( state
                |> selectHelp ((==) a)
                |> (\(State newStuff) ->
                        if closeAfterMouseSelection then
                            State { newStuff | open = False }
                        else
                            State { newStuff | keyboardFocus = Just index }
                   )
            , if closeAfterMouseSelection then
                focusButton id
              else
                Cmd.none
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
