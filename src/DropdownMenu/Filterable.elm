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
        , adjustScrollTop
        , allowDefault
        , appendAttributes
        , centerScrollTop
        , computeRenderedEntries
        , domIndex
        , find
        , findNext
        , findPrevious
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
type State a
    = State
        { query : String
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
closed : State a
closed =
    State
        { query = ""
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
    viewHelp cfg ids state selection filteredEntries <|
        { spaceAboveFirst = 0
        , droppedAboveFirst = 0
        , spaceAboveSecond = 0
        , droppedAboveSecond = 0
        , spaceBelowFirst = 0
        , droppedBelowFirst = 0
        , spaceBelowSecond = 0
        , droppedBelowSecond = 0
        , entriesAbove = []
        , visibleEntries = filteredEntries
        , entriesBelow = []
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
viewLazy entryHeight ((Config { matchesQuery, entryId }) as cfg) ids selection ((State stuff) as state) entries =
    let
        filteredEntries =
            entries
                |> List.filter (matchesQuery stuff.query)

        maybeFocusIndex =
            stuff.keyboardFocus
                |> Maybe.andThen
                    (\focus ->
                        find entryId focus filteredEntries
                    )
                |> Maybe.map Tuple.first
    in
    filteredEntries
        |> computeRenderedEntries entryHeight
            stuff.ulScrollTop
            stuff.ulClientHeight
            maybeFocusIndex
        |> viewHelp cfg ids state selection filteredEntries


viewHelp :
    Config a
    ->
        { id : String
        , labelledBy : String
        , placeholder : String
        }
    -> State a
    -> Maybe a
    -> List a
    -> RenderedEntries a
    -> Html (Msg a)
viewHelp (Config cfg) ids (State stuff) selection allEntries renderedEntries =
    let
        displayed =
            List.length renderedEntries.visibleEntries
    in
    Html.div
        (appendAttributes NoOp cfg.container [])
        [ Html.input
            ([ Attributes.id (printTextfieldId ids.id)
             , Attributes.type_ "text"
             , Attributes.attribute "aria-haspopup" "listbox"
             , Attributes.attribute "aria-labelledby"
                (printTextfieldId ids.id ++ " " ++ ids.labelledBy)
             , Attributes.style "position" "relative"
             , Attributes.tabindex 0
             , selection
                |> Maybe.map cfg.printEntry
                |> Maybe.withDefault ids.placeholder
                |> Attributes.placeholder
             , Attributes.value stuff.query
             , Events.onClick (TextfieldClicked ids.id)
             , Events.onInput (TextfieldChanged ids.id)
             , Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (textfieldKeydown ids.id
                            cfg.jumpAtEnds
                            cfg.entryId
                            stuff.open
                            stuff.keyboardFocus
                            allEntries
                            renderedEntries.droppedAboveFirst
                            renderedEntries.droppedAboveSecond
                            renderedEntries.droppedBelowFirst
                            displayed
                        )
                )
             , Events.on "blur" <|
                Decode.succeed TextfieldBlured
             , Events.preventDefaultOn "keypress"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "Home" ->
                                    ListHomePressed ids.id cfg.entryId allEntries
                                        |> Decode.succeed
                                        |> preventDefault

                                "End" ->
                                    ListEndPressed ids.id cfg.entryId allEntries
                                        |> Decode.succeed
                                        |> preventDefault

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )
             ]
                |> setAriaExpanded stuff.open
                |> appendAttributes NoOp
                    (cfg.textfield
                        { selection = selection
                        , open = stuff.open
                        }
                    )
            )
            []
        , Html.ul
            ([ Attributes.id (printListId ids.id)
             , Attributes.attribute "role" "listbox"
             , Attributes.attribute "aria-labelledby" ids.labelledBy
             , Attributes.style "position" "absolute"
             , Events.on "mousedown" (Decode.succeed (ListMouseDown ids.id))
             , Events.on "mouseup" (Decode.succeed ListMouseUp)
             , Events.on "scroll" <|
                Decode.map2 (ListScrolled ids.id)
                    (Decode.at [ "target", "scrollTop" ] Decode.float)
                    (Decode.at [ "target", "clientHeight" ] Decode.float)
             ]
                |> setDisplay stuff.open
                |> setAriaActivedescendant
                    ids.id
                    cfg.entryId
                    stuff.keyboardFocus
                    allEntries
                |> appendAttributes NoOp cfg.ul
            )
            (viewEntries
                { entryMouseEntered = EntryMouseEntered
                , entryMouseLeft = EntryMouseLeft
                , entryClicked = EntryClicked
                , noOp = NoOp
                }
                cfg
                ids.id
                stuff.keyboardFocus
                stuff.mouseFocus
                selection
                renderedEntries
            )
        ]


textfieldKeydown :
    String
    -> Bool
    -> (a -> String)
    -> Bool
    -> Maybe String
    -> List a
    -> Int
    -> Int
    -> Int
    -> Int
    -> String
    -> Decoder ( Msg a, Bool )
textfieldKeydown id jumpAtEnds entryId open keyboardFocus allEntries droppedAboveFirst droppedAboveSecond droppedBelowFirst displayed code =
    case code of
        "Enter" ->
            keyboardFocus
                |> Maybe.andThen
                    (\currentFocus ->
                        find entryId currentFocus allEntries
                    )
                |> Maybe.map
                    (\( _, entry ) ->
                        Decode.succeed (TextfieldEnterPressed entry)
                            |> allowDefault
                    )
                |> Maybe.withDefault
                    (Decode.fail "not handling that key here")

        "Escape" ->
            Decode.succeed TextfieldEscapePressed
                |> allowDefault

        " " ->
            if open then
                Decode.succeed NoOp
                    |> allowDefault
            else
                Decode.succeed (TextfieldSpacePressed id)
                    |> preventDefault

        "ArrowUp" ->
            let
                focusLastEntry =
                    case last allEntries of
                        Nothing ->
                            Decode.fail "not handling that key here"

                        Just lastEntry ->
                            Decode.succeed <|
                                TextfieldArrowPressed Nothing ScrollToBottom id (entryId lastEntry)
            in
            case keyboardFocus of
                Nothing ->
                    preventDefault <|
                        focusLastEntry

                Just currentFocus ->
                    case findPrevious entryId currentFocus allEntries of
                        Just (Last lastEntry) ->
                            if jumpAtEnds then
                                preventDefault <|
                                    Decode.succeed <|
                                        TextfieldArrowPressed Nothing
                                            ScrollToBottom
                                            id
                                            (entryId lastEntry)
                            else
                                preventDefault <|
                                    Decode.succeed <|
                                        TextfieldArrowPressed Nothing ScrollToTop id currentFocus

                        Just (Previous newIndex newEntry) ->
                            domIndex newIndex
                                droppedAboveFirst
                                droppedAboveSecond
                                droppedBelowFirst
                                displayed
                                |> scrollDataDecoder
                                |> Decode.map
                                    (\scrollData ->
                                        TextfieldArrowPressed (Just scrollData)
                                            UseScrollData
                                            id
                                            (entryId newEntry)
                                    )
                                |> preventDefault

                        Nothing ->
                            focusLastEntry
                                |> preventDefault

        "ArrowDown" ->
            let
                focusFirstEntry =
                    case List.head allEntries of
                        Nothing ->
                            Decode.fail "not handling that key here"

                        Just lastEntry ->
                            Decode.succeed <|
                                TextfieldArrowPressed Nothing ScrollToTop id (entryId lastEntry)
            in
            case keyboardFocus of
                Nothing ->
                    preventDefault <|
                        focusFirstEntry

                Just currentFocus ->
                    case findNext entryId currentFocus allEntries of
                        Just (First firstEntry) ->
                            if jumpAtEnds then
                                preventDefault <|
                                    Decode.succeed <|
                                        TextfieldArrowPressed Nothing
                                            ScrollToTop
                                            id
                                            (entryId firstEntry)
                            else
                                preventDefault <|
                                    Decode.succeed <|
                                        TextfieldArrowPressed Nothing ScrollToBottom id currentFocus

                        Just (Next newIndex newEntry) ->
                            domIndex newIndex
                                droppedAboveFirst
                                droppedAboveSecond
                                droppedBelowFirst
                                displayed
                                |> scrollDataDecoder
                                |> Decode.map
                                    (\scrollData ->
                                        TextfieldArrowPressed (Just scrollData)
                                            UseScrollData
                                            id
                                            (entryId newEntry)
                                    )
                                |> preventDefault

                        Nothing ->
                            focusFirstEntry
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
    | TextfieldClicked String
    | TextfieldChanged String String
    | TextfieldSpacePressed String
    | TextfieldArrowPressed (Maybe ScrollData) ScrollAction String String
    | TextfieldEnterPressed a
    | TextfieldEscapePressed
    | TextfieldBlured
      -- LIST
    | ListMouseDown String
    | ListMouseUp
    | ListScrolled String Float Float
    | ListHomePressed String (a -> String) (List a)
    | ListEndPressed String (a -> String) (List a)
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String Bool String a


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
            , resetScrollTop NoOp id stuff.keyboardFocus stuff.scrollDataCache
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
            , resetScrollTop NoOp id stuff.keyboardFocus stuff.scrollDataCache
            , Nothing
            )

        TextfieldArrowPressed maybeScrollData scrollAction id newFocus ->
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
                                |> Maybe.map (centerScrollTop NoOp id)
                                |> Maybe.withDefault
                                    (Browser.scrollIntoView (printEntryId id newFocus)
                                        |> Task.attempt (\_ -> NoOp)
                                    )

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
            , Nothing
            )

        TextfieldEnterPressed a ->
            ( State
                { stuff
                    | open = False
                    , keyboardFocus = Nothing
                    , query = ""
                }
            , Cmd.none
            , Just (lifts.entrySelected a)
            )

        TextfieldEscapePressed ->
            ( State
                { stuff
                    | open = False
                    , query = ""
                }
            , Cmd.none
            , Nothing
            )

        TextfieldBlured ->
            ( State
                { stuff
                    | open =
                        if stuff.preventBlur then
                            stuff.open
                        else
                            False
                    , query = ""
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

        ListScrolled id ulScrollTop ulClientHeight ->
            ( State
                { stuff
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , focusTextfield id
            , Nothing
            )

        ListHomePressed id entryId entries ->
            ( State
                { stuff
                    | keyboardFocus =
                        entries
                            |> List.head
                            |> Maybe.map entryId
                }
            , Browser.setScrollTop (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
            , Nothing
            )

        ListEndPressed id entryId entries ->
            ( State
                { stuff
                    | keyboardFocus =
                        entries
                            |> List.reverse
                            |> List.head
                            |> Maybe.map entryId
                }
            , Browser.setScrollBottom (printListId id) 0
                |> Task.attempt (\_ -> NoOp)
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


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)
