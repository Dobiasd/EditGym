module Editor (..) where

import Keyboard
import Set
import String
import Dict
import Regex
import Maybe
import List exposing ((::))
import Text
import Char exposing (fromCode)
import Color exposing (Color)
import Unsafe exposing (unsafeTail)
import Layout exposing (toDefText, white1, darkGray1)
import Graphics.Element exposing (Element, spacer, container, flow, down, right, outward, empty, widthOf, heightOf, color, leftAligned)


type alias Document =
    String


type alias Cursor =
    Int



-- (from, to), from can be > to, == means no selection


type alias Selection =
    ( Int, Int )


type alias State =
    { document : Document
    , cursor : Cursor
    , selection : Selection
    , clipboard : String
    }


setDocument : String -> State -> State
setDocument newDoc state =
    { state | document = newDoc }


boolToInt : Bool -> Int
boolToInt b =
    case b of
        False ->
            0

        True ->
            1


keyStrings : Dict.Dict ( Int, Char.KeyCode ) Char
keyStrings =
    let
        toDefStrPair shift key c = ( ( shift, key ), c |> fromCode )
    in
        [ ( ( 0, 13 ), '\n' )
        , ( ( 1, 13 ), '\n' )
        , ( ( 0, 32 ), ' ' )
        , ( ( 1, 32 ), ' ' )
        , ( ( 0, 188 ), ',' )
        , ( ( 0, 190 ), '.' )
        ]
            ++
                (List.map2 (toDefStrPair 0) [48..57] [48..57])
            -- top numbers
            ++
                (List.map2 (toDefStrPair 0) [96..105] [48..57])
            -- block numbers
            ++
                (List.map2 (toDefStrPair 0) [65..90] [97..122])
            -- a to z
            ++
                (List.map2 (toDefStrPair 1) [65..90] [65..90])
            -- A to Z
            |>
                Dict.fromList


tabStr : String
tabStr =
    "    "


showKey : Bool -> Char.KeyCode -> Maybe Char
showKey shift key =
    Dict.get ( boolToInt shift, key ) keyStrings


initialState : State
initialState =
    State "" 0 ( 0, 0 ) ""


setSnd : Int -> ( Int, Int ) -> ( Int, Int )
setSnd x ( a, b ) =
    ( a, x )


setBoth : Int -> ( Int, Int )
setBoth x =
    ( x, x )


stepSelection : Bool -> State -> State
stepSelection shift ({ cursor, selection } as state) =
    { state
        | selection =
            if shift then
                setSnd cursor selection
            else
                setBoth cursor
    }



-- for ctrl+right


isWordEnd : Char -> Char -> Bool
isWordEnd a b =
    case ( a, b ) of
        ( ' ', _ ) ->
            False

        ( _, ' ' ) ->
            True

        ( _, ',' ) ->
            True

        ( _, '.' ) ->
            True

        ( _, '\n' ) ->
            True

        ( '\n', _ ) ->
            True

        ( _, _ ) ->
            False



-- for ctrl+left


isWordStart : Char -> Char -> Bool
isWordStart =
    flip isWordEnd


genCharPairs : String -> List ( Char, Char )
genCharPairs str =
    List.map2
        (,)
        (str |> String.toList)
        (str |> String.dropLeft 1 |> String.toList)


wordStarts : Document -> List Cursor
wordStarts doc =
    let
        markers =
            genCharPairs doc
                |> List.indexedMap (\idx ( a, b ) -> ( idx + 1, isWordStart a b ))
    in
        ( 0, True ) :: markers |> List.filter snd |> List.map fst


wordEnds : Document -> List Cursor
wordEnds doc =
    let
        markers =
            genCharPairs doc
                |> List.indexedMap (\idx ( a, b ) -> ( idx + 1, isWordEnd a b ))
    in
        markers ++ [ ( String.length doc, True ) ] |> List.filter snd |> List.map fst


find : (a -> Bool) -> List a -> Maybe a
find cond xs =
    case xs of
        x :: xs ->
            if cond x then
                Just x
            else
                find cond xs

        [] ->
            Nothing


lineEnds : Document -> List Cursor
lineEnds =
    String.toList
        >> List.indexedMap (\idx x -> ( idx, x == '\n' ))
        >> List.filter snd
        >> List.map fst


lineStarts : Document -> List Cursor
lineStarts doc =
    lineEnds doc |> List.map (\idx -> idx + 1)


wordLeftOffset : Document -> Cursor -> Int
wordLeftOffset document cursor =
    let
        startPositions = document |> wordStarts
    in
        case List.reverse startPositions |> find (\x -> x < cursor) of
            Just pos ->
                pos - cursor

            Nothing ->
                0


wordRightOffset : Document -> Cursor -> Int
wordRightOffset document cursor =
    let
        endPositions = document |> wordEnds
    in
        case endPositions |> find (\x -> x > cursor) of
            Just pos ->
                pos - cursor

            Nothing ->
                0


lineLeftOffset : Document -> Cursor -> Int
lineLeftOffset document cursor =
    let
        positions = 0 :: lineStarts document
    in
        case List.reverse positions |> find (\x -> x <= cursor) of
            Just pos ->
                pos - cursor

            Nothing ->
                0


lineRightOffset : Document -> Cursor -> Int
lineRightOffset document cursor =
    let
        positions = lineEnds document ++ [ String.length document ]
    in
        case positions |> find (\x -> x >= cursor) of
            Just pos ->
                pos - cursor

            Nothing ->
                String.length document - cursor


stepCursorLeft : Bool -> Bool -> State -> State
stepCursorLeft ctrl shift ({ document, cursor, selection } as state) =
    let
        dist =
            if ctrl then
                wordLeftOffset document cursor
            else if isSelected selection && not shift then
                uncurry min selection - cursor
            else
                -1

        cursor' = max 0 (cursor + dist)
    in
        { state | cursor = cursor' } |> stepSelection shift


stepCursorRight : Bool -> Bool -> State -> State
stepCursorRight ctrl shift ({ document, cursor, selection } as state) =
    let
        dist =
            if ctrl then
                wordRightOffset document cursor
            else if isSelected selection && not shift then
                uncurry max selection - cursor
            else
                1

        cursor' = min (String.length document) (cursor + dist)
    in
        { state | cursor = cursor' } |> stepSelection shift


stepCursorUp : Bool -> Bool -> State -> State
stepCursorUp ctrl shift ({ document, cursor, selection } as state) =
    let
        x = cursor - (stepCursorPos1 False False state).cursor

        aboveStartState =
            state
                |> stepCursorPos1 False False
                |> stepCursorLeft False False
                |> stepCursorPos1 False False

        aboveStart = aboveStartState.cursor

        aboveEnd =
            aboveStartState
                |> stepCursorEnd False False
                |> .cursor

        cursor' = min (aboveStart + x) aboveEnd
    in
        case ( ctrl, shift ) of
            ( True, _ ) ->
                state

            otherwise ->
                { state | cursor = cursor' } |> stepSelection shift


stepCursorDown : Bool -> Bool -> State -> State
stepCursorDown ctrl shift ({ document, cursor, selection } as state) =
    let
        x = cursor - (stepCursorPos1 False False state).cursor

        belowStartState =
            state
                |> stepCursorEnd False False
                |> stepCursorRight False False

        belowStart = belowStartState.cursor

        aboveEnd =
            belowStartState
                |> stepCursorEnd False False
                |> .cursor

        cursor' = min (belowStart + x) aboveEnd
    in
        case ( ctrl, shift ) of
            ( True, _ ) ->
                state

            otherwise ->
                { state | cursor = cursor' } |> stepSelection shift


stepCursorPos1 : Bool -> Bool -> State -> State
stepCursorPos1 ctrl shift ({ document, cursor, selection } as state) =
    let
        dist =
            if ctrl then
                -cursor
            else
                lineLeftOffset document cursor

        cursor' = max 0 (cursor + dist)
    in
        { state | cursor = cursor' } |> stepSelection shift


stepCursorEnd : Bool -> Bool -> State -> State
stepCursorEnd ctrl shift ({ document, cursor, selection } as state) =
    let
        dist =
            if ctrl then
                (String.length document - cursor)
            else
                lineRightOffset document cursor

        cursor' = min (String.length document) (cursor + dist)
    in
        { state | cursor = cursor' } |> stepSelection shift


stepCursor : Bool -> Bool -> Char.KeyCode -> State -> State
stepCursor ctrl shift key ({ document, cursor } as state) =
    (case key of
        35 ->
            stepCursorEnd

        36 ->
            stepCursorPos1

        37 ->
            stepCursorLeft

        39 ->
            stepCursorRight

        38 ->
            stepCursorUp

        40 ->
            stepCursorDown

        otherwise ->
            (\_ _ state -> state)
    )
        ctrl
        shift
        state


isSelected : Selection -> Bool
isSelected ( start, end ) =
    start /= end


sortPair : ( Int, Int ) -> ( Int, Int )
sortPair ( a, b ) =
    if a <= b then
        ( a, b )
    else
        ( b, a )


deleteSelection : State -> State
deleteSelection ({ document, selection, cursor } as state) =
    let
        ( start, end ) = sortPair selection

        part1 = String.slice 0 start document

        part2 = String.slice end (String.length document) document
    in
        { state
            | document = String.append part1 part2
            , selection = setBoth start
            , cursor = start
        }


setCursorAndSelection : Cursor -> State -> State
setCursorAndSelection pos state =
    { state
        | cursor = pos
        , selection = setBoth pos
    }


stepBackspace : Bool -> Bool -> Char.KeyCode -> State -> State
stepBackspace ctrl shift key ({ selection, cursor } as state) =
    let
        stepF =
            if isSelected selection then
                identity
            else
                case ( ctrl, shift ) of
                    ( True, True ) ->
                        stepCursorPos1 False True

                    ( True, False ) ->
                        stepCursorLeft True True

                    ( False, _ ) ->
                        stepCursorLeft False True
    in
        case key of
            8 ->
                stepF state |> deleteSelection

            otherwise ->
                state


stepDelete : Bool -> Bool -> Char.KeyCode -> State -> State
stepDelete ctrl shift key ({ selection } as state) =
    let
        stepF =
            if isSelected selection then
                identity
            else
                case ( ctrl, shift ) of
                    ( True, True ) ->
                        stepCursorEnd False True

                    ( True, False ) ->
                        stepCursorRight True True

                    ( False, _ ) ->
                        stepCursorRight False True
    in
        case key of
            46 ->
                stepF state |> deleteSelection

            otherwise ->
                state


stepCopy : Bool -> Bool -> Char.KeyCode -> State -> State
stepCopy ctrl shift key ({ document, selection, clipboard } as state) =
    if (ctrl && key == 67) || (ctrl && key == 45) then
        if isSelected selection then
            { state
                | clipboard =
                    uncurry
                        String.slice
                        (sortPair selection)
                        document
            }
        else
            state
    else
        state


stepPaste : Bool -> Bool -> Char.KeyCode -> State -> State
stepPaste ctrl shift key ({ document, selection, clipboard } as state) =
    if (ctrl && key == 86) || (shift && key == 45) then
        replaceSelection clipboard state
    else
        state


stepCut : Bool -> Bool -> Char.KeyCode -> State -> State
stepCut ctrl shift key ({ document, selection, clipboard } as state) =
    if (ctrl && key == 88) || (shift && key == 46) then
        state |> stepCopy True False 67 |> deleteSelection
    else
        state


stepTab : Bool -> Bool -> Char.KeyCode -> State -> State
stepTab ctrl shift key ({ document, selection, clipboard } as state) =
    if (not ctrl && key == 9) then
        replaceSelection tabStr state
    else
        state


stepSelectAll : Bool -> Bool -> Char.KeyCode -> State -> State
stepSelectAll ctrl shift key ({ document, selection } as state) =
    if (ctrl && key == 65) then
        { state | selection = ( 0, String.length document ) }
    else
        state


replaceSelection : String -> State -> State
replaceSelection str ({ document, selection } as state) =
    let
        state' = deleteSelection state

        ( part1, part2 ) = splitAtCursor state'
    in
        { state'
            | document = String.concat [ part1, str, part2 ]
            , cursor = (String.length <| (String.concat [ part1, str ]))
        }
            |> stepSelection False


stepType : Bool -> Char.KeyCode -> State -> State
stepType shift key state =
    case showKey shift key of
        Just c ->
            replaceSelection (String.fromList [ c ]) state

        Nothing ->
            state


stepSpecialType : Bool -> Bool -> Char.KeyCode -> State -> State
stepSpecialType ctrl shift key state =
    if ctrl && key == 13 then
        replaceSelection "\n" state
    else
        state


splitAtCursor : State -> ( Document, Document )
splitAtCursor { document, cursor } =
    ( String.slice 0 cursor document
    , String.slice cursor (String.length document) document
    )


applyWith : List (a -> b -> b) -> a -> b -> b
applyWith fs x =
    let
        halfAppliedFs = List.map (\f -> f x) fs
    in
        List.foldl (>>) identity halfAppliedFs


step : State -> Set.Set Char.KeyCode -> List Char.KeyCode -> State
step ({ document, cursor } as state) keysDown keysDownNew =
    let
        keysDownAll = Set.union keysDown (Set.fromList keysDownNew)

        shift = Set.member 16 keysDownAll

        ctrl = Set.member 17 keysDownAll

        stepKey =
            applyWith
                [ stepBackspace ctrl shift
                , stepDelete ctrl shift
                , if ctrl then
                    (flip always)
                  else
                    stepType shift
                , stepCursor ctrl shift
                , stepCopy ctrl shift
                , stepPaste ctrl shift
                , stepCut ctrl shift
                , stepSelectAll ctrl shift
                , stepTab ctrl shift
                , stepSpecialType ctrl shift
                ]
    in
        List.foldl stepKey state keysDownNew


replace : String -> String -> String -> String
replace token replacement =
    Regex.replace Regex.All (Regex.regex token) (\_ -> replacement)


replaceAllButNewlines : Char -> String -> String
replaceAllButNewlines replacement str =
    let
        transF c =
            if c == '\n' then
                '\n'
            else
                replacement
    in
        str |> String.toList |> List.map transF |> String.fromList


safeHead : a -> List a -> a
safeHead def xs =
    case xs of
        x :: xs ->
            x

        otherwise ->
            def


safeLast : a -> List a -> a
safeLast def =
    List.reverse >> safeHead def


init : List a -> List a
init =
    List.reverse >> unsafeTail >> List.reverse


displayCursor : Document -> Cursor -> Element
displayCursor document cursor =
    let
        upToCursor = document |> String.left cursor

        rows = upToCursor |> String.split "\n"

        lastRow = rows |> safeLast ""

        dummyStr = "H"

        yText =
            if String.isEmpty lastRow then
                String.append upToCursor dummyStr
            else
                upToCursor

        y =
            yText
                |> displayTextCol white1
                |> heightOf
                |> \x -> x - cursorHeight

        x = lastRow |> displayTextCol white1 |> widthOf

        cursorHeight = displayTextCol white1 dummyStr |> heightOf
    in
        flow
            down
            [ spacer 1 y
            , flow
                right
                [ spacer x 1
                , spacer 1 cursorHeight |> color white1
                ]
            ]


displaySelection : Document -> Selection -> Element
displaySelection document selection =
    let
        ( begin, end ) = sortPair selection

        str =
            document
                |> replaceAllButNewlines ' '
                |> String.toList
                |> List.indexedMap transF
                |> List.concat
                |> String.fromList

        transF idx c =
            let
                isSelected = idx >= begin && idx < end
            in
                case c of
                    '\n' ->
                        if isSelected then
                            [ '█', '\n' ]
                        else
                            [ '\n' ]

                    otherwise ->
                        if isSelected then
                            [ '█' ]
                        else
                            [ c ]
    in
        str |> displayTextCol darkGray1


displaySpaces : Document -> Element
displaySpaces document =
    let
        str = document |> replace " " "•"
    in
        str |> displayTextCol darkGray1


displayTextCol : Color -> Document -> Element
displayTextCol col =
    Text.fromString
        >> Text.typeface [ "Consolas", "Menlo Regular", "Monospace" ]
        >> Text.height 20
        >> Text.color col
        >> leftAligned


displayNewLines : Document -> Element
displayNewLines =
    replaceAllButNewlines ' '
        >> replace "\n" "n\n"
        >> displayTextCol darkGray1


displayDocument : Color -> Document -> Element
displayDocument col document =
    flow
        outward
        [ displayNewLines document
        , displaySpaces document
        , displayTextCol col document
        ]


display : State -> Element
display ({ document, cursor, selection, clipboard } as state) =
    flow
        outward
        [ if isSelected selection then
            displaySelection document selection
          else
            empty
        , displayCursor document cursor
        , displayDocument white1 document
        ]
