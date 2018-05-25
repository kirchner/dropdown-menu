module DropdownMenu.Simple
    exposing
        ( Behaviour
        , Config
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Msg
        , State
        , View
        , closed
        , subscriptions
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
        , findWithQuery
        , findWithQueryFromTop
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
    , mouseFocus : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type Query
    = NoQuery
    | Query Time.Posix String


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
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , handleTypeAhead : Maybe (a -> String)
    }


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
    case state of
        Closed ->
            viewClosed config ids allEntries selection

        Open { keyboardFocus, mouseFocus } ->
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewButton config ids allEntries selection True
                , viewList config ids keyboardFocus mouseFocus selection allEntries <|
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
    case state of
        Closed ->
            viewClosed config ids allEntries selection

        Open { keyboardFocus, mouseFocus, ulScrollTop, ulClientHeight } ->
            let
                maybeFocusIndex =
                    find config.uniqueId keyboardFocus allEntries
                        |> Maybe.map Tuple.first
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewButton config ids allEntries selection True
                , allEntries
                    |> computeRenderedEntries entryHeight ulScrollTop ulClientHeight maybeFocusIndex
                    |> viewList config ids keyboardFocus mouseFocus selection allEntries
                ]


viewClosed : Config a -> Ids -> List a -> Maybe a -> Html (Msg a)
viewClosed config ids allEntries selection =
    Html.div
        (appendAttributes NoOp config.view.container [])
        [ viewButton config ids allEntries selection False ]


viewButton : Config a -> Ids -> List a -> Maybe a -> Bool -> Html (Msg a)
viewButton config ids allEntries selection open =
    let
        { attributes, children } =
            config.view.button
                { selection = selection
                , open = open
                }
    in
    Html.button
        ([ Attributes.id (printButtonId ids.id)
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printButtonId ids.id ++ " " ++ ids.labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , Events.onClick (ButtonClicked config.behaviour ids.id config.uniqueId allEntries)
         , Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown config.behaviour ids.id config.uniqueId allEntries)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes NoOp attributes
        )
        (List.map (Html.map (\_ -> NoOp)) children)


viewList :
    Config a
    -> Ids
    -> String
    -> Maybe String
    -> Maybe a
    -> List a
    -> RenderedEntries a
    -> Html (Msg a)
viewList config ids keyboardFocus maybeMouseFocus selection allEntries renderedEntries =
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
                    (listKeydown config ids keyboardFocus allEntries renderedEntries.visibleEntries)
            )
         , Events.on "blur" (Decode.succeed ListBlured)
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         ]
            |> handleKeypress config ids allEntries
            |> setAriaActivedescendant ids.id config.uniqueId (Just keyboardFocus) allEntries
            |> appendAttributes NoOp config.view.ul
        )
        (viewEntries
            { entryMouseEntered = EntryMouseEntered
            , entryMouseLeft = EntryMouseLeft
            , entryClicked =
                \_ closeAfterMouseSelection _ a ->
                    EntryClicked ids.id config.uniqueId closeAfterMouseSelection a
            , noOp = NoOp
            }
            { closeAfterMouseSelection = config.behaviour.closeAfterMouseSelection
            , li = config.view.li
            , entryId = config.uniqueId
            }
            ids.id
            (Just keyboardFocus)
            maybeMouseFocus
            selection
            renderedEntries
        )


buttonKeyDown : Behaviour a -> String -> (a -> String) -> List a -> String -> Decoder (Msg a)
buttonKeyDown behaviour id entryId allEntries code =
    case code of
        "ArrowUp" ->
            Decode.succeed <|
                ButtonArrowUpPressed behaviour id entryId allEntries

        "ArrowDown" ->
            Decode.succeed <|
                ButtonArrowDownPressed behaviour id entryId allEntries

        _ ->
            Decode.fail "not handling that key here"


listKeydown :
    Config a
    -> Ids
    -> String
    -> List a
    -> List a
    -> String
    -> Decoder ( Msg a, Bool )
listKeydown { uniqueId, behaviour } { id } keyboardFocus allEntries visibleEntries code =
    case code of
        "ArrowUp" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed behaviour id uniqueId allEntries)
                |> preventDefault

        "ArrowDown" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed behaviour id uniqueId allEntries)
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
    Config a
    -> Ids
    -> List a
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
handleKeypress { uniqueId, behaviour } ids allEntries attrs =
    Events.on "keypress"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\code ->
                    case code of
                        "Home" ->
                            if behaviour.handleHomeAndEnd then
                                Decode.succeed <|
                                    ListHomePressed ids.id uniqueId allEntries
                            else
                                Decode.fail "not handling that key here"

                        "End" ->
                            if behaviour.handleHomeAndEnd then
                                Decode.succeed <|
                                    ListEndPressed ids.id uniqueId allEntries
                            else
                                Decode.fail "not handling that key here"

                        _ ->
                            case behaviour.handleTypeAhead of
                                Nothing ->
                                    Decode.fail "not handling that key here"

                                Just entryToString ->
                                    if String.length code == 1 then
                                        ListKeyPressed ids.id uniqueId allEntries entryToString code
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
    | ButtonClicked (Behaviour a) String (a -> String) (List a)
    | ButtonArrowUpPressed (Behaviour a) String (a -> String) (List a)
    | ButtonArrowDownPressed (Behaviour a) String (a -> String) (List a)
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowUpPressed (Behaviour a) String (a -> String) (List a) (Maybe ScrollData)
    | ListArrowDownPressed (Behaviour a) String (a -> String) (List a) (Maybe ScrollData)
    | ListEnterPressed String (a -> String) (List a)
    | ListEscapePressed String
    | ListBlured
    | ListHomePressed String (a -> String) (List a)
    | ListEndPressed String (a -> String) (List a)
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String (a -> String) Bool a
      -- QUERY
    | ListKeyPressed String (a -> String) (List a) (a -> String) String
    | CurrentTimeReceived String (a -> String) (List a) (a -> String) String Time.Posix
    | Tick Time.Posix


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
        ButtonClicked behaviour id entryId allEntries ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = entryId firstEntry
                        , mouseFocus = Nothing
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

        ButtonArrowUpPressed behaviour id entryId allEntries ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just lastEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = entryId lastEntry
                        , mouseFocus = Nothing
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

        ButtonArrowDownPressed behaviour id entryId allEntries ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = NoQuery
                        , keyboardFocus = entryId firstEntry
                        , mouseFocus = Nothing
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

        ListArrowUpPressed behaviour id uniqueId allEntries maybeScrollData ->
            case findPrevious uniqueId stuff.keyboardFocus allEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        ( Open { stuff | keyboardFocus = uniqueId lastEntry }
                        , scrollListToBottom id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected lastEntry)
                          else
                            Nothing
                        )
                    else
                        ( Open stuff
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newIndex newEntry) ->
                    ( Open { stuff | keyboardFocus = uniqueId newEntry }
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

        ListArrowDownPressed behaviour id uniqueId allEntries maybeScrollData ->
            case findNext uniqueId stuff.keyboardFocus allEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        ( Open { stuff | keyboardFocus = uniqueId firstEntry }
                        , scrollListToTop id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected firstEntry)
                          else
                            Nothing
                        )
                    else
                        ( Open stuff
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newIndex newEntry) ->
                    ( Open { stuff | keyboardFocus = uniqueId newEntry }
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

        ListHomePressed id entryId allEntries ->
            case List.head allEntries of
                Nothing ->
                    ( Closed
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    ( Open { stuff | keyboardFocus = entryId firstEntry }
                    , scrollListToTop id
                    , Nothing
                    )

        ListEndPressed id entryId allEntries ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open { stuff | keyboardFocus = entryId firstEntry }
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
        EntryMouseEntered newFocus ->
            ( Open { stuff | mouseFocus = Just newFocus }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( Open { stuff | mouseFocus = Nothing }
            , Cmd.none
            , Nothing
            )

        EntryClicked id entryId closeAfterMouseSelection a ->
            ( if closeAfterMouseSelection then
                Closed
              else
                Open { stuff | keyboardFocus = entryId a }
            , if closeAfterMouseSelection then
                focusButton id
              else
                Cmd.none
            , Just (entrySelected a)
            )

        -- QUERY
        ListKeyPressed id entryId entries entryToString code ->
            ( Open stuff
            , Time.now
                |> Task.perform (CurrentTimeReceived id entryId entries entryToString code)
            , Nothing
            )

        CurrentTimeReceived id entryId entries entryToString code currentTime ->
            let
                ( newQuery, queryText ) =
                    case stuff.query of
                        NoQuery ->
                            ( Query currentTime code, code )

                        Query _ query ->
                            ( Query currentTime (query ++ code), query ++ code )

                newKeyboardFocus =
                    findWithQuery entryId stuff.keyboardFocus queryText entryToString entries
            in
            case newKeyboardFocus of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just newFocus ->
                    ( Open
                        { stuff
                            | query = newQuery
                            , keyboardFocus = newFocus
                        }
                    , Browser.scrollIntoView (printEntryId id newFocus)
                        |> Task.attempt (\_ -> NoOp)
                    , Nothing
                    )

        Tick currentTime ->
            ( case stuff.query of
                NoQuery ->
                    Open stuff

                Query time _ ->
                    if Time.posixToMillis currentTime - Time.posixToMillis time > 1000 then
                        Open { stuff | query = NoQuery }
                    else
                        Open stuff
            , Cmd.none
            , Nothing
            )

        _ ->
            ( Open stuff, Cmd.none, Nothing )


subscriptions : State -> Sub (Msg a)
subscriptions state =
    case state of
        Closed ->
            Sub.none

        Open stuff ->
            case stuff.query of
                NoQuery ->
                    Sub.none

                Query _ _ ->
                    Time.every 300 Tick



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
