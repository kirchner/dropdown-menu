module DropdownMenu.Simple
    exposing
        ( Config
        , Msg
        , State
        , closed
        , config
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
    entries
        |> computeRenderedEntries entryHeight
            stuff.ulScrollTop
            stuff.ulClientHeight
            maybeFocusIndex
        |> viewHelp cfg ids state selection entries


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

        { attributes, children } =
            cfg.button
                { selection = selection
                , open = stuff.open
                }
    in
    Html.div
        (appendAttributes NoOp cfg.container [])
        [ Html.button
            ([ Attributes.id (printButtonId id)
             , Attributes.attribute "aria-haspopup" "listbox"
             , Attributes.attribute "aria-labelledby"
                (printButtonId id ++ " " ++ labelledBy)
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
                |> appendAttributes NoOp attributes
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
                |> appendAttributes NoOp cfg.ul
            )
            (viewEntries
                { entryMouseEntered = EntryMouseEntered
                , entryMouseLeft = EntryMouseLeft
                , entryClicked = EntryClicked
                , noOp = NoOp
                }
                cfg
                id
                stuff.keyboardFocus
                stuff.mouseFocus
                selection
                renderedEntries
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
                    domIndex newIndex
                        droppedAboveFirst
                        droppedAboveSecond
                        droppedBelowFirst
                        displayed
                        |> scrollDataDecoder
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
                    domIndex newIndex
                        droppedAboveFirst
                        droppedAboveSecond
                        droppedBelowFirst
                        displayed
                        |> scrollDataDecoder
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
    | ButtonClicked String
    | ButtonArrowPressed ScrollAction String String
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListArrowPressed (Maybe ScrollData) ScrollAction String String
    | ListEnterPressed String a
    | ListEscapePressed String
    | ListBlured
    | ListScrolled Float Float
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String Bool String a


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
            , [ resetScrollTop NoOp id stuff.keyboardFocus stuff.scrollDataCache
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
                            |> Maybe.map (centerScrollTop NoOp id)
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
                                |> Maybe.map (centerScrollTop NoOp id)
                                |> Maybe.withDefault
                                    (Browser.scrollIntoView (printEntryId id newFocus)
                                        |> Task.attempt (\_ -> NoOp)
                                    )

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
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


type OutMsg a
    = EntrySelected a


{-| -}
updateOptional : Msg a -> Maybe a -> State a -> ( State a, Maybe a, Cmd (Msg a) )
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
updateRequired : Msg a -> a -> State a -> ( State a, a, Cmd (Msg a) )
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
