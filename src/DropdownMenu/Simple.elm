module DropdownMenu.Simple
    exposing
        ( Behaviour
        , Config
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Msg
        , State
        , TypeAhead
        , View
        , closed
        , noTypeAhead
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , update
        , updateOptional
        , updateRequired
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
import Internal.Shared
    exposing
        ( Next(..)
        , Previous(..)
        , RenderedEntries
        , ScrollAction(..)
        , ScrollData
        , adjustScrollTop
        , allowDefault
        , appendAttributes
        , centerScrollTop
        , computeRenderedEntries
        , domIndex
        , find
        , findNext
        , findPrevious
        , findWith
        , indexOf
        , last
        , preventDefault
        , printEntryId
        , printListId
        , resetScrollTop
        , setAriaActivedescendant
        , setAriaExpanded
        , setDisplay
        , viewEntries
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Task
import Time


{-| -}
type State
    = Closed
    | Open OpenData


type alias OpenData =
    { preventBlur : Bool
    , query : Query

    -- FOCUS
    , keyboardFocus : String
    , maybeMouseFocus : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type Query
    = NoQuery
    | Query Int Time.Posix String


{-| -}
closed : State
closed =
    Closed



---- CONFIG


{-| -}
type alias Config a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    , view : View a
    }


{-| -}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    }


{-| -}
type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)


{-| -}
noTypeAhead : TypeAhead a
noTypeAhead =
    NoTypeAhead


{-| -}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead timeout entryToString =
    TypeAhead timeout <|
        \query entry ->
            String.toLower (entryToString entry)
                |> String.startsWith (String.toLower query)


{-| -}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead


{-| -}
type alias View a =
    { container : HtmlAttributes
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
        , maybeQuery : Maybe String
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



---- VIEW


{-| -}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| -}
view : Config a -> Ids -> State -> List a -> Maybe a -> Html (Msg a)
view config ids state allEntries selection =
    let
        data =
            { behaviour = config.behaviour
            , id = ids.id
            , uniqueId = config.uniqueId
            , allEntries = allEntries
            }

        buttonHtmlDetails =
            config.view.button
                { selection = selection
                , open = True
                }
    in
    case state of
        Closed ->
            viewClosed data config.view.container buttonHtmlDetails ids.labelledBy selection

        Open { keyboardFocus, maybeMouseFocus, query } ->
            let
                maybeQuery =
                    case query of
                        NoQuery ->
                            Nothing

                        Query _ _ actualQuery ->
                            Just actualQuery
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewButton data buttonHtmlDetails ids.labelledBy selection True
                , viewList data config ids maybeQuery keyboardFocus maybeMouseFocus selection <|
                    { spaceAboveFirst = 0
                    , droppedAboveFirst = 0
                    , spaceAboveSecond = 0
                    , droppedAboveSecond = 0
                    , spaceBelowFirst = 0
                    , droppedBelowFirst = 0
                    , spaceBelowSecond = 0
                    , droppedBelowSecond = 0
                    , entriesAbove = []
                    , visibleEntries = allEntries
                    , entriesBelow = []
                    }
                ]


{-| -}
viewLazy : (a -> Float) -> Config a -> Ids -> State -> List a -> Maybe a -> Html (Msg a)
viewLazy entryHeight config ids state allEntries selection =
    let
        data =
            { behaviour = config.behaviour
            , id = ids.id
            , uniqueId = config.uniqueId
            , allEntries = allEntries
            }

        buttonHtmlDetails =
            config.view.button
                { selection = selection
                , open = True
                }
    in
    case state of
        Closed ->
            viewClosed data config.view.container buttonHtmlDetails ids.labelledBy selection

        Open { keyboardFocus, maybeMouseFocus, ulScrollTop, ulClientHeight, query } ->
            let
                maybeFocusIndex =
                    find config.uniqueId keyboardFocus allEntries
                        |> Maybe.map Tuple.first

                maybeQuery =
                    case query of
                        NoQuery ->
                            Nothing

                        Query _ _ actualQuery ->
                            Just actualQuery
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewButton data buttonHtmlDetails ids.labelledBy selection True
                , allEntries
                    |> computeRenderedEntries entryHeight ulScrollTop ulClientHeight maybeFocusIndex
                    |> viewList data config ids maybeQuery keyboardFocus maybeMouseFocus selection
                ]


viewClosed : Data a -> HtmlAttributes -> HtmlDetails -> String -> Maybe a -> Html (Msg a)
viewClosed data containerHtmlAttributes buttonHtmlDetails labelledBy selection =
    Html.div
        (appendAttributes NoOp containerHtmlAttributes [])
        [ viewButton data buttonHtmlDetails labelledBy selection False ]


viewButton : Data a -> HtmlDetails -> String -> Maybe a -> Bool -> Html (Msg a)
viewButton ({ id } as data) { attributes, children } labelledBy selection open =
    Html.button
        ([ Attributes.id (printButtonId id)
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printButtonId id ++ " " ++ labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , Events.onClick (ButtonClicked data)
         , Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown data)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes NoOp attributes
        )
        (List.map (Html.map (\_ -> NoOp)) children)


viewList :
    Data a
    -> Config a
    -> Ids
    -> Maybe String
    -> String
    -> Maybe String
    -> Maybe a
    -> RenderedEntries a
    -> Html (Msg a)
viewList data config ids maybeQuery keyboardFocus maybeMouseFocus selection renderedEntries =
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.style "position" "absolute"
         , Attributes.tabindex -1
         , Events.on "mousedown" (Decode.succeed ListMouseDown)
         , Events.on "mouseup" (Decode.succeed ListMouseUp)
         , Events.preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (listKeydown data keyboardFocus renderedEntries.visibleEntries)
            )
         , Events.on "blur" (Decode.succeed ListBlured)
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         ]
            |> handleKeypress data
            |> setAriaActivedescendant ids.id config.uniqueId (Just keyboardFocus) data.allEntries
            |> appendAttributes NoOp config.view.ul
        )
        (viewEntries
            { entryMouseEntered = EntryMouseEntered config.behaviour
            , entryMouseLeft = EntryMouseLeft config.behaviour
            , entryClicked =
                \_ closeAfterMouseSelection _ a ->
                    EntryClicked config.behaviour ids.id config.uniqueId closeAfterMouseSelection a
            , noOp = NoOp
            }
            { closeAfterMouseSelection = config.behaviour.closeAfterMouseSelection
            , li = config.view.li
            , entryId = config.uniqueId
            }
            ids.id
            maybeQuery
            (Just keyboardFocus)
            maybeMouseFocus
            selection
            renderedEntries
        )


buttonKeyDown : Data a -> String -> Decoder (Msg a)
buttonKeyDown data code =
    case code of
        "ArrowUp" ->
            Decode.succeed (ButtonArrowUpPressed data)

        "ArrowDown" ->
            Decode.succeed (ButtonArrowDownPressed data)

        _ ->
            Decode.fail "not handling that key here"


listKeydown :
    Data a
    -> String
    -> List a
    -> String
    -> Decoder ( Msg a, Bool )
listKeydown ({ id, uniqueId, behaviour, allEntries } as data) keyboardFocus visibleEntries code =
    case code of
        "ArrowUp" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed data)
                |> preventDefault

        "ArrowDown" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed data)
                |> preventDefault

        "Enter" ->
            Decode.succeed (ListEnterPressed id uniqueId allEntries)
                |> preventDefault

        "Escape" ->
            Decode.succeed (ListEscapePressed id)
                |> allowDefault

        " " ->
            Decode.succeed NoOp
                |> preventDefault

        _ ->
            Decode.fail "not handling that key here"


handleKeypress :
    Data a
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
handleKeypress ({ uniqueId, behaviour, id, allEntries } as data) attrs =
    Events.on "keypress"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\code ->
                    case code of
                        "Home" ->
                            if behaviour.handleHomeAndEnd then
                                Decode.succeed (ListHomePressed data)
                            else
                                Decode.fail "not handling that key here"

                        "End" ->
                            if behaviour.handleHomeAndEnd then
                                Decode.succeed (ListEndPressed data)
                            else
                                Decode.fail "not handling that key here"

                        _ ->
                            case behaviour.typeAhead of
                                NoTypeAhead ->
                                    Decode.fail "not handling that key here"

                                TypeAhead timeout matchesQuery ->
                                    if String.length code == 1 then
                                        ListKeyPressed data timeout matchesQuery code
                                            |> Decode.succeed
                                    else
                                        Decode.fail "not handling that key here"
                )
        )
        :: attrs


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


printButtonId : String -> String
printButtonId id =
    id ++ "__button"



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- BUTTON
    | ButtonClicked (Data a)
    | ButtonArrowUpPressed (Data a)
    | ButtonArrowDownPressed (Data a)
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowUpPressed (Data a) (Maybe ScrollData)
    | ListArrowDownPressed (Data a) (Maybe ScrollData)
    | ListEnterPressed String (a -> String) (List a)
    | ListEscapePressed String
    | ListBlured
    | ListHomePressed (Data a)
    | ListEndPressed (Data a)
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered (Behaviour a) String
    | EntryMouseLeft (Behaviour a)
    | EntryClicked (Behaviour a) String (a -> String) Bool a
      -- QUERY
    | ListKeyPressed (Data a) Int (String -> a -> Bool) String
    | CurrentTimeReceived (Data a) Int (String -> a -> Bool) String Time.Posix
    | Tick Time.Posix


type alias Data a =
    { behaviour : Behaviour a
    , id : String
    , uniqueId : a -> String
    , allEntries : List a
    }


{-| -}
update : (a -> outMsg) -> State -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
update entrySelected state msg =
    case state of
        Closed ->
            updateClosed entrySelected msg

        Open stuff ->
            updateOpen entrySelected stuff msg


updateClosed : (a -> outMsg) -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
updateClosed entrySelected msg =
    case msg of
        NoOp ->
            ( Closed, Cmd.none, Nothing )

        -- BUTTON
        ButtonClicked { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , Cmd.batch
                        [ scrollListToTop id
                        , focusList id
                        ]
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        ButtonArrowUpPressed { behaviour, id, uniqueId, allEntries } ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just lastEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = uniqueId lastEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId lastEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , Cmd.batch
                        [ scrollListToBottom id
                        , focusList id
                        ]
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected lastEntry)
                      else
                        Nothing
                    )

        ButtonArrowDownPressed { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , Cmd.batch
                        [ scrollListToTop id
                        , focusList id
                        ]
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        _ ->
            ( Closed, Cmd.none, Nothing )


updateOpen : (a -> outMsg) -> OpenData -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
updateOpen entrySelected stuff msg =
    case msg of
        -- LIST
        ListMouseDown ->
            ( Open { stuff | preventBlur = True }
            , Cmd.none
            , Nothing
            )

        ListMouseUp ->
            ( Open { stuff | preventBlur = False }
            , Cmd.none
            , Nothing
            )

        ListArrowUpPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case findPrevious uniqueId stuff.keyboardFocus allEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        ( { stuff | query = NoQuery }
                            |> updateKeyboardFocus behaviour (uniqueId lastEntry)
                            |> Open
                        , scrollListToBottom id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected lastEntry)
                          else
                            Nothing
                        )
                    else
                        ( Open { stuff | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newIndex newEntry) ->
                    ( { stuff | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId newEntry)
                        |> Open
                    , case maybeScrollData of
                        Nothing ->
                            Browser.scrollIntoView (printEntryId id (uniqueId newEntry))
                                |> Task.attempt (\_ -> NoOp)

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected newEntry)
                      else
                        Nothing
                    )

                Nothing ->
                    ( Closed
                    , Cmd.none
                    , Nothing
                    )

        ListArrowDownPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case findNext uniqueId stuff.keyboardFocus allEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        ( { stuff | query = NoQuery }
                            |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                            |> Open
                        , scrollListToTop id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected firstEntry)
                          else
                            Nothing
                        )
                    else
                        ( Open { stuff | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newIndex newEntry) ->
                    ( { stuff | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId newEntry)
                        |> Open
                    , case maybeScrollData of
                        Nothing ->
                            Browser.scrollIntoView (printEntryId id (uniqueId newEntry))
                                |> Task.attempt (\_ -> NoOp)

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected newEntry)
                      else
                        Nothing
                    )

                Nothing ->
                    ( Closed
                    , Cmd.none
                    , Nothing
                    )

        ListEnterPressed id uniqueId allEntries ->
            ( Closed
            , focusButton id
            , case find uniqueId stuff.keyboardFocus allEntries of
                Nothing ->
                    Nothing

                Just ( _, a ) ->
                    Just (entrySelected a)
            )

        ListEscapePressed id ->
            ( Closed
            , focusButton id
            , Nothing
            )

        ListBlured ->
            if stuff.preventBlur then
                ( Open stuff
                , Cmd.none
                , Nothing
                )
            else
                ( Closed
                , Cmd.none
                , Nothing
                )

        ListHomePressed { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Closed
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    ( { stuff | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Open
                    , scrollListToTop id
                    , Nothing
                    )

        ListEndPressed { behaviour, id, uniqueId, allEntries } ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( { stuff | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Open
                    , scrollListToBottom id
                    , Nothing
                    )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Open
                { stuff
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered behaviour newFocus ->
            ( Open
                { stuff
                    | keyboardFocus =
                        if behaviour.separateFocus then
                            stuff.keyboardFocus
                        else
                            newFocus
                    , maybeMouseFocus = Just newFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft behaviour ->
            ( Open
                { stuff
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing
                        else
                            stuff.maybeMouseFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryClicked behaviour id uniqueId closeAfterMouseSelection a ->
            ( if closeAfterMouseSelection then
                Closed
              else
                { stuff | query = NoQuery }
                    |> updateKeyboardFocus behaviour (uniqueId a)
                    |> Open
            , if closeAfterMouseSelection then
                focusButton id
              else
                Cmd.none
            , Just (entrySelected a)
            )

        -- QUERY
        ListKeyPressed data timeout matchesQuery code ->
            ( Open stuff
            , Time.now
                |> Task.perform
                    (CurrentTimeReceived data timeout matchesQuery code)
            , Nothing
            )

        CurrentTimeReceived { behaviour, id, uniqueId, allEntries } timeout matchesQuery code currentTime ->
            let
                ( newQuery, queryText ) =
                    case stuff.query of
                        NoQuery ->
                            ( Query timeout currentTime code, code )

                        Query _ _ query ->
                            ( Query timeout currentTime (query ++ code), query ++ code )

                newKeyboardFocus =
                    findWith matchesQuery uniqueId stuff.keyboardFocus queryText allEntries
            in
            case newKeyboardFocus of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just newFocus ->
                    ( { stuff | query = newQuery }
                        |> updateKeyboardFocus behaviour newFocus
                        |> Open
                    , Browser.scrollIntoView (printEntryId id newFocus)
                        |> Task.attempt (\_ -> NoOp)
                    , Nothing
                    )

        Tick currentTime ->
            ( case stuff.query of
                NoQuery ->
                    Open stuff

                Query timeout time _ ->
                    if Time.posixToMillis currentTime - Time.posixToMillis time > timeout then
                        Open { stuff | query = NoQuery }
                    else
                        Open stuff
            , Cmd.none
            , Nothing
            )

        _ ->
            ( Open stuff, Cmd.none, Nothing )


updateKeyboardFocus : { b | separateFocus : Bool } -> String -> OpenData -> OpenData
updateKeyboardFocus { separateFocus } newFocus stuff =
    { stuff
        | keyboardFocus = newFocus
        , maybeMouseFocus =
            if separateFocus then
                stuff.maybeMouseFocus
            else
                Just newFocus
    }


{-| -}
subscriptions : State -> Sub (Msg a)
subscriptions state =
    case state of
        Closed ->
            Sub.none

        Open stuff ->
            case stuff.query of
                NoQuery ->
                    Sub.none

                Query timeout _ _ ->
                    Time.every (toFloat (timeout // 3)) Tick



-- SIMPLER UPDATES


type OutMsg a
    = EntrySelected a


{-| -}
updateOptional : Msg a -> Maybe a -> State -> ( State, Maybe a, Cmd (Msg a) )
updateOptional msg selection state =
    let
        ( newState, cmd, maybeOutMsg ) =
            update EntrySelected state msg
    in
    case maybeOutMsg of
        Nothing ->
            ( newState, selection, cmd )

        Just (EntrySelected a) ->
            ( newState, Just a, cmd )


{-| -}
updateRequired : Msg a -> a -> State -> ( State, a, Cmd (Msg a) )
updateRequired msg selection state =
    let
        ( newState, cmd, maybeOutMsg ) =
            update EntrySelected state msg
    in
    case maybeOutMsg of
        Nothing ->
            ( newState, selection, cmd )

        Just (EntrySelected a) ->
            ( newState, a, cmd )



-- CMDS


focusList : String -> Cmd (Msg a)
focusList id =
    Browser.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


focusButton : String -> Cmd (Msg a)
focusButton id =
    Browser.focus (printButtonId id)
        |> Task.attempt (\_ -> NoOp)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Browser.setScrollTop (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Browser.setScrollBottom (printListId id) 0
        |> Task.attempt (\_ -> NoOp)
