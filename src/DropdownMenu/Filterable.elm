module DropdownMenu.Filterable
    exposing
        ( Behaviour
        , Config
        , Msg
        , State
        , View
        , closed
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


{-| -}
type State
    = Closed
    | OpenEmpty String
    | Open OpenData


type alias OpenData =
    { preventBlur : Bool
    , query : String

    -- FOCUS
    , keyboardFocus : String
    , maybeMouseFocus : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


{-| -}
closed : State
closed =
    Closed



---- CONFIG


{-| -}
type alias Config a =
    { uniqueId : a -> String
    , printEntry : a -> String
    , matchesQuery : String -> a -> Bool
    , behaviour : Behaviour
    , view : View a
    }


{-| -}
type alias Behaviour =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    }


{-| -}
type alias View a =
    { container : HtmlAttributes
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
    , placeholder : String
    }


{-| -}
view : Config a -> Ids -> State -> List a -> Maybe a -> Html (Msg a)
view config ids state allEntries selection =
    let
        textfieldHtmlAttributes =
            config.view.textfield
                { selection = selection
                , open = True
                }
    in
    case state of
        Closed ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries = allEntries
                    , matchesQuery = config.matchesQuery
                    }
            in
            viewClosed data
                config.printEntry
                ids.placeholder
                config.view.container
                textfieldHtmlAttributes
                ids.labelledBy
                selection
                allEntries

        OpenEmpty query ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries = []
                    , matchesQuery = config.matchesQuery
                    }
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewTextfield data
                    config.printEntry
                    ids.placeholder
                    query
                    Nothing
                    textfieldHtmlAttributes
                    ids.labelledBy
                    selection
                    []
                    True
                ]

        Open { keyboardFocus, maybeMouseFocus, query } ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries =
                        allEntries
                            |> List.filter (config.matchesQuery query)
                    , matchesQuery = config.matchesQuery
                    }
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewTextfield data
                    config.printEntry
                    ids.placeholder
                    query
                    (Just keyboardFocus)
                    textfieldHtmlAttributes
                    ids.labelledBy
                    selection
                    data.filteredEntries
                    True
                , viewList data config ids (Just query) keyboardFocus maybeMouseFocus selection <|
                    { spaceAboveFirst = 0
                    , droppedAboveFirst = 0
                    , spaceAboveSecond = 0
                    , droppedAboveSecond = 0
                    , spaceBelowFirst = 0
                    , droppedBelowFirst = 0
                    , spaceBelowSecond = 0
                    , droppedBelowSecond = 0
                    , entriesAbove = []
                    , visibleEntries = data.filteredEntries
                    , entriesBelow = []
                    }
                ]


{-| -}
viewLazy : (a -> Float) -> Config a -> Ids -> State -> List a -> Maybe a -> Html (Msg a)
viewLazy entryHeight config ids state allEntries selection =
    let
        textfieldHtmlAttributes =
            config.view.textfield
                { selection = selection
                , open = True
                }
    in
    case state of
        Closed ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries = allEntries
                    , matchesQuery = config.matchesQuery
                    }
            in
            viewClosed data
                config.printEntry
                ids.placeholder
                config.view.container
                textfieldHtmlAttributes
                ids.labelledBy
                selection
                allEntries

        OpenEmpty query ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries = []
                    , matchesQuery = config.matchesQuery
                    }
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewTextfield data
                    config.printEntry
                    ids.placeholder
                    query
                    Nothing
                    textfieldHtmlAttributes
                    ids.labelledBy
                    selection
                    []
                    True
                ]

        Open { keyboardFocus, maybeMouseFocus, query, ulScrollTop, ulClientHeight } ->
            let
                data =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    , filteredEntries =
                        allEntries
                            |> List.filter (config.matchesQuery query)
                    , matchesQuery = config.matchesQuery
                    }

                maybeFocusIndex =
                    find config.uniqueId keyboardFocus data.filteredEntries
                        |> Maybe.map Tuple.first

                renderedEntries =
                    computeRenderedEntries
                        entryHeight
                        ulScrollTop
                        ulClientHeight
                        maybeFocusIndex
                        data.filteredEntries
            in
            Html.div
                (appendAttributes NoOp config.view.container [])
                [ viewTextfield data
                    config.printEntry
                    ids.placeholder
                    query
                    (Just keyboardFocus)
                    textfieldHtmlAttributes
                    ids.labelledBy
                    selection
                    renderedEntries.visibleEntries
                    True
                , viewList
                    data
                    config
                    ids
                    (Just query)
                    keyboardFocus
                    maybeMouseFocus
                    selection
                    renderedEntries
                ]


viewClosed :
    Data a
    -> (a -> String)
    -> String
    -> HtmlAttributes
    -> HtmlAttributes
    -> String
    -> Maybe a
    -> List a
    -> Html (Msg a)
viewClosed data printEntry placeholder containerHtmlAttributes textfieldHtmlAttributes labelledBy selection visibleEntries =
    Html.div
        (appendAttributes NoOp containerHtmlAttributes [])
        [ viewTextfield data
            printEntry
            placeholder
            ""
            Nothing
            textfieldHtmlAttributes
            labelledBy
            selection
            visibleEntries
            False
        ]


viewTextfield :
    Data a
    -> (a -> String)
    -> String
    -> String
    -> Maybe String
    -> HtmlAttributes
    -> String
    -> Maybe a
    -> List a
    -> Bool
    -> Html (Msg a)
viewTextfield ({ id } as data) printEntry placeholder query maybeKeyboardFocus attributes labelledBy selection visibleEntries open =
    Html.input
        ([ Attributes.id (printTextfieldId id)
         , Attributes.type_ "text"
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printTextfieldId id ++ " " ++ labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , selection
            |> Maybe.map printEntry
            |> Maybe.withDefault placeholder
            |> Attributes.placeholder
         , Attributes.value query
         , Events.onClick (TextfieldClicked data)
         , Events.onInput (TextfieldChanged data)
         , Events.on "blur" (Decode.succeed TextfieldBlured)
         , Events.preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (textfieldKeydown data maybeKeyboardFocus visibleEntries open)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes NoOp attributes
        )
        []


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
         , Events.on "mousedown" (Decode.succeed (ListMouseDown ids.id))
         , Events.on "mouseup" (Decode.succeed ListMouseUp)
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         ]
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


textfieldKeydown : Data a -> Maybe String -> List a -> Bool -> String -> Decoder ( Msg a, Bool )
textfieldKeydown data maybeKeyboardFocus visibleEntries open code =
    let
        { id, uniqueId, behaviour, allEntries } =
            data
    in
    case code of
        "ArrowUp" ->
            case maybeKeyboardFocus of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just keyboardFocus ->
                    Decode.oneOf
                        [ visibleEntries
                            |> indexOf uniqueId keyboardFocus
                            |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                        , Decode.succeed Nothing
                        ]
                        |> Decode.map (TextfieldArrowUpPressed data)
                        |> preventDefault

        "ArrowDown" ->
            case maybeKeyboardFocus of
                Nothing ->
                    Decode.fail "not handling that key here"

                Just keyboardFocus ->
                    Decode.oneOf
                        [ visibleEntries
                            |> indexOf uniqueId keyboardFocus
                            |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                        , Decode.succeed Nothing
                        ]
                        |> Decode.map (TextfieldArrowDownPressed data)
                        |> preventDefault

        "Enter" ->
            Decode.succeed (TextfieldEnterPressed uniqueId allEntries)
                |> preventDefault

        "Escape" ->
            Decode.succeed TextfieldEscapePressed
                |> allowDefault

        " " ->
            if open then
                Decode.succeed NoOp
                    |> allowDefault
            else
                Decode.succeed (TextfieldSpacePressed data)
                    |> preventDefault

        "Home" ->
            Decode.succeed (TextfieldHomePressed data)
                |> preventDefault

        "End" ->
            Decode.succeed (TextfieldEndPressed data)
                |> preventDefault

        _ ->
            Decode.fail "not handling that key here"


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



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- TEXTFIELD
    | TextfieldClicked (Data a)
    | TextfieldChanged (Data a) String
    | TextfieldSpacePressed (Data a)
    | TextfieldArrowUpPressed (Data a) (Maybe ScrollData)
    | TextfieldArrowDownPressed (Data a) (Maybe ScrollData)
    | TextfieldEnterPressed (a -> String) (List a)
    | TextfieldEscapePressed
    | TextfieldBlured
    | TextfieldHomePressed (Data a)
    | TextfieldEndPressed (Data a)
      -- LIST
    | ListMouseDown String
    | ListMouseUp
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered Behaviour String
    | EntryMouseLeft Behaviour
    | EntryClicked Behaviour String (a -> String) Bool a


type alias Data a =
    { behaviour : Behaviour
    , id : String
    , uniqueId : a -> String
    , allEntries : List a
    , filteredEntries : List a
    , matchesQuery : String -> a -> Bool
    }


{-| -}
update : (a -> outMsg) -> State -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
update entrySelected state msg =
    case state of
        Closed ->
            updateClosed entrySelected msg

        OpenEmpty query ->
            updateOpenEmpty entrySelected query msg

        Open stuff ->
            updateOpen entrySelected stuff msg


updateClosed : (a -> outMsg) -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
updateClosed entrySelected msg =
    case msg of
        -- BUTTON
        TextfieldClicked { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = ""
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , scrollListToTop id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        TextfieldSpacePressed { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = ""
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , scrollListToTop id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        TextfieldArrowUpPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just lastEntry ->
                    ( Open
                        { preventBlur = False
                        , query = ""
                        , keyboardFocus = uniqueId lastEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId lastEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , scrollListToBottom id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected lastEntry)
                      else
                        Nothing
                    )

        TextfieldArrowDownPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case List.head allEntries of
                Nothing ->
                    ( Closed, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = ""
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , scrollListToTop id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        _ ->
            ( Closed, Cmd.none, Nothing )


updateOpenEmpty : (a -> outMsg) -> String -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
updateOpenEmpty entrySelected query msg =
    case msg of
        TextfieldChanged { behaviour, id, uniqueId, allEntries, matchesQuery } newQuery ->
            let
                filteredEntries =
                    List.filter (matchesQuery newQuery) allEntries
            in
            case List.head filteredEntries of
                Nothing ->
                    ( OpenEmpty newQuery, Cmd.none, Nothing )

                Just firstEntry ->
                    ( Open
                        { preventBlur = False
                        , query = newQuery
                        , keyboardFocus = uniqueId firstEntry
                        , maybeMouseFocus =
                            if behaviour.separateFocus then
                                Nothing
                            else
                                Just (uniqueId firstEntry)
                        , ulScrollTop = 0
                        , ulClientHeight = 1000
                        }
                    , scrollListToTop id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        TextfieldEnterPressed _ _ ->
            ( Closed
            , Cmd.none
            , Nothing
            )

        TextfieldEscapePressed ->
            ( Closed
            , Cmd.none
            , Nothing
            )

        TextfieldBlured ->
            ( Closed
            , Cmd.none
            , Nothing
            )

        _ ->
            ( OpenEmpty query, Cmd.none, Nothing )


updateOpen : (a -> outMsg) -> OpenData -> Msg a -> ( State, Cmd (Msg a), Maybe outMsg )
updateOpen entrySelected stuff msg =
    case msg of
        TextfieldChanged { behaviour, id, uniqueId, allEntries, matchesQuery } newQuery ->
            let
                filteredEntries =
                    List.filter (matchesQuery newQuery) allEntries
            in
            case List.head filteredEntries of
                Nothing ->
                    ( OpenEmpty newQuery, Cmd.none, Nothing )

                Just firstEntry ->
                    ( { stuff | query = newQuery }
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Open
                    , scrollListToTop id
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected firstEntry)
                      else
                        Nothing
                    )

        TextfieldArrowUpPressed { behaviour, id, uniqueId, allEntries, matchesQuery, filteredEntries } maybeScrollData ->
            case findPrevious uniqueId stuff.keyboardFocus filteredEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        ( stuff
                            |> updateKeyboardFocus behaviour (uniqueId lastEntry)
                            |> Open
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
                    ( stuff
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
                    ( OpenEmpty stuff.query
                    , Cmd.none
                    , Nothing
                    )

        TextfieldArrowDownPressed { behaviour, id, uniqueId, allEntries, matchesQuery, filteredEntries } maybeScrollData ->
            case findNext uniqueId stuff.keyboardFocus filteredEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        ( stuff
                            |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                            |> Open
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
                    ( stuff
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
                    ( OpenEmpty stuff.query
                    , Cmd.none
                    , Nothing
                    )

        TextfieldEnterPressed uniqueId allEntries ->
            ( Closed
            , Cmd.none
            , case find uniqueId stuff.keyboardFocus allEntries of
                Nothing ->
                    Nothing

                Just ( _, a ) ->
                    Just (entrySelected a)
            )

        TextfieldEscapePressed ->
            ( Closed
            , Cmd.none
            , Nothing
            )

        TextfieldBlured ->
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

        TextfieldHomePressed { behaviour, id, uniqueId, allEntries, matchesQuery, filteredEntries } ->
            case List.head filteredEntries of
                Nothing ->
                    ( OpenEmpty stuff.query
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    ( stuff
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Open
                    , scrollListToTop id
                    , Nothing
                    )

        TextfieldEndPressed { behaviour, id, uniqueId, allEntries, matchesQuery, filteredEntries } ->
            case List.head (List.reverse filteredEntries) of
                Nothing ->
                    ( OpenEmpty stuff.query, Cmd.none, Nothing )

                Just firstEntry ->
                    ( stuff
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Open
                    , scrollListToBottom id
                    , Nothing
                    )

        -- LIST
        ListMouseDown id ->
            ( Open { stuff | preventBlur = True }
            , focusTextfield id
            , Nothing
            )

        ListMouseUp ->
            ( Open { stuff | preventBlur = False }
            , Cmd.none
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
                stuff
                    |> updateKeyboardFocus behaviour (uniqueId a)
                    |> Open
            , focusTextfield id
            , Just (entrySelected a)
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


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Browser.setScrollTop (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Browser.setScrollBottom (printListId id) 0
        |> Task.attempt (\_ -> NoOp)
