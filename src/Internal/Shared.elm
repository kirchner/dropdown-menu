module Internal.Shared
    exposing
        ( Next(..)
        , Previous(..)
        , RenderedEntries
        , ScrollData
        , adjustScrollTop
        , allowDefault
        , appendAttributes
        , computeRenderedEntries
        , find
        , findNext
        , findPrevious
        , findWith
        , indexOf
        , preventDefault
        , printEntryId
        , printListId
        , setAriaActivedescendant
        , setAriaExpanded
        , viewEntries
        )

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Task


---- VIEW


{-| -}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }


viewEntries :
    { entryMouseEntered : String -> msg
    , entryMouseLeft : msg
    , entryClicked : String -> (a -> String) -> Bool -> a -> msg
    , noOp : msg
    , closeAfterMouseSelection : Bool
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , uniqueId : a -> String
    }
    -> String
    -> Maybe String
    -> String
    -> Maybe String
    -> Maybe a
    -> RenderedEntries a
    -> List (Html msg)
viewEntries cfg id maybeQuery keyboardFocus maybeMouseFocus selection renderedEntries =
    List.concat
        [ spacer renderedEntries.spaceAboveFirst
        , renderedEntries.entriesAbove
            |> List.map
                (\a ->
                    viewEntry cfg
                        id
                        maybeQuery
                        (selection == Just a)
                        (keyboardFocus == cfg.uniqueId a)
                        (maybeMouseFocus == Just (cfg.uniqueId a))
                        a
                )
        , spacer renderedEntries.spaceAboveSecond
        , renderedEntries.visibleEntries
            |> List.map
                (\a ->
                    viewEntry cfg
                        id
                        maybeQuery
                        (selection == Just a)
                        (keyboardFocus == cfg.uniqueId a)
                        (maybeMouseFocus == Just (cfg.uniqueId a))
                        a
                )
        , spacer renderedEntries.spaceBelowFirst
        , renderedEntries.entriesBelow
            |> List.map
                (\a ->
                    viewEntry cfg
                        id
                        maybeQuery
                        (selection == Just a)
                        (keyboardFocus == cfg.uniqueId a)
                        (maybeMouseFocus == Just (cfg.uniqueId a))
                        a
                )
        , spacer renderedEntries.spaceBelowSecond
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


viewEntry :
    { entryMouseEntered : String -> msg
    , entryMouseLeft : msg
    , entryClicked : String -> (a -> String) -> Bool -> a -> msg
    , noOp : msg
    , closeAfterMouseSelection : Bool
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , uniqueId : a -> String
    }
    -> String
    -> Maybe String
    -> Bool
    -> Bool
    -> Bool
    -> a
    -> Html msg
viewEntry cfg id maybeQuery selected keyboardFocused mouseFocused a =
    let
        { attributes, children } =
            cfg.li
                { selected = selected
                , keyboardFocused = keyboardFocused
                , mouseFocused = mouseFocused
                , maybeQuery = maybeQuery
                }
                a
    in
    Html.li
        ([ Events.onMouseEnter (cfg.entryMouseEntered (cfg.uniqueId a))
         , Events.onMouseLeave cfg.entryMouseLeft
         , Events.onClick (cfg.entryClicked id cfg.uniqueId cfg.closeAfterMouseSelection a)
         , Attributes.id (printEntryId id (cfg.uniqueId a))
         , Attributes.attribute "role" "option"
         ]
            |> appendAttributes cfg.noOp attributes
        )
        (children
            |> List.map (Html.map (\_ -> cfg.noOp))
        )



---- VIEW HELPER


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded isOpen attrs =
    if isOpen then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs


setAriaActivedescendant :
    String
    -> (a -> String)
    -> String
    -> List a
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id uniqueId keyboardFocus entries attrs =
    entries
        |> find uniqueId keyboardFocus
        |> Maybe.map
            (\( _, focusedEntry ) ->
                Attributes.attribute "aria-activedescendant"
                    (printEntryId id (uniqueId focusedEntry))
                    :: attrs
            )
        |> Maybe.withDefault attrs



---- COMPUTE RENDERED ENTRIES


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



---- FIND CURRENT/NEXT/PREVIOUS ENTRIES


indexOf : (a -> String) -> String -> List a -> Maybe Int
indexOf =
    indexOfHelp 0


indexOfHelp : Int -> (a -> String) -> String -> List a -> Maybe Int
indexOfHelp index uniqueId id entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == id then
                Just index
            else
                indexOfHelp (index + 1) uniqueId id rest


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


findWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
findWith matchesQuery uniqueId focus query entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == focus then
                let
                    id =
                        uniqueId entry
                in
                if matchesQuery query entry then
                    Just id
                else
                    proceedWith matchesQuery uniqueId id query rest
            else
                findWith matchesQuery uniqueId focus query rest


proceedWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        next :: rest ->
            if matchesQuery query next then
                Just (uniqueId next)
            else
                proceedWith matchesQuery uniqueId id query rest


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
                entries
                    |> List.reverse
                    |> List.head
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



-- MISC


appendAttributes :
    msg
    -> List (Html.Attribute Never)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
appendAttributes noOp neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> noOp))
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))



---- IDS


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



-- CMDS


type alias ScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


adjustScrollTop : msg -> String -> ScrollData -> Cmd msg
adjustScrollTop noOp id ({ ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } as scrollData) =
    if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
        if liOffsetTop <= ulScrollTop + ulClientHeight then
            Browser.setScrollTop (printListId id)
                (liOffsetTop + liOffsetHeight - ulClientHeight)
                |> Task.attempt (\_ -> noOp)
        else
            centerScrollTop noOp id scrollData
    else if liOffsetTop < ulScrollTop then
        if liOffsetTop + liOffsetHeight >= ulScrollTop then
            Browser.setScrollTop (printListId id) liOffsetTop
                |> Task.attempt (\_ -> noOp)
        else
            centerScrollTop noOp id scrollData
    else
        Cmd.none


centerScrollTop : msg -> String -> ScrollData -> Cmd msg
centerScrollTop noOp id { ulClientHeight, liOffsetTop, liOffsetHeight } =
    Browser.setScrollTop (printListId id)
        (liOffsetTop + liOffsetHeight / 2 - ulClientHeight / 2)
        |> Task.attempt (\_ -> noOp)
