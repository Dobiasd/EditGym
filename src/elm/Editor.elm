module Editor where

import Keyboard
import Set
import String
import Debug -- todo raus
import Dict
import Maybe
import List
import Text
import Char(fromCode)

import Layout (toDefText, white1, darkGray1)

type Document = String
type Cursor = Int

-- (from, to), from can be > to, == means no selection
type Selection = (Int, Int)

type State = {
    document : Document
  , cursor : Cursor
  , selection : Selection
  }

boolToInt : Bool -> Int
boolToInt b = case b of
  False -> 0
  True -> 1

keyStrings : Dict.Dict (Int, Keyboard.KeyCode) Char
keyStrings =
  let toDefStrPair shift mod key = ((shift, mod key), key |> fromCode)
  in  [
    ((0, 13), '\n')
  , ((1, 13), '\n')
  , ((0, 32), ' ')
  , ((1, 32), ' ')
  , ((0, 188), ',')
  , ((0, 190), '.')
  ]
  ++ (map (toDefStrPair 0 identity) [48..57]) -- top numbers
  ++ (map (toDefStrPair 0 identity) [96..105]) -- block numbers
  ++ (map (toDefStrPair 0 (\x -> x - 32)) [97..122]) -- a to z
  ++ (map (toDefStrPair 1 identity) [65..90]) -- A to Z
  |> Dict.fromList

showKey : Bool -> Keyboard.KeyCode -> Maybe Char
showKey shift key = Dict.get (boolToInt shift, key) keyStrings

initialState : State
initialState = State "" 0 (0, 0)

setSnd : Int -> (Int, Int) -> (Int, Int)
setSnd x (a, b) = (a, x)

setBoth : Int -> (Int, Int)
setBoth x = (x, x)

stepSelection : Bool -> State -> State
stepSelection shift ({cursor, selection} as state) =
  { state | selection <- if shift then setSnd cursor selection
                                  else setBoth cursor }

isWordStart : Char -> Char -> Bool
isWordStart a b =
  case (a, b) of
    (_, ' ') -> False
    (_, ',') -> True
    (_, '.') -> True
    (' ', _) -> True
    (_, '\n') -> True
    ('\n', _) -> True
    (_, _) -> False

wordStarts : Document -> [Cursor]
wordStarts doc =
  let markers = zip
                  (doc |> String.toList)
                  (doc |> String.dropLeft 1 |> String.toList)
                |> List.indexedMap (\idx (a, b) -> (idx + 1, isWordStart a b))
  in  ((0, True)::markers) ++ [(String.length doc, True)]
      |> filter snd |> map fst

find : (a -> Bool) -> [a] -> Maybe a
find cond xs =
  case xs of
    (x::xs) -> if cond x then Just x else find cond xs
    [] -> Nothing

lineEnds : Document -> [Cursor]
lineEnds =
  String.toList
  >> List.indexedMap (\idx x -> (idx, x == '\n'))
  >> filter snd
  >> map fst

lineStarts : Document -> [Cursor]
lineStarts doc =
  lineEnds doc |> map (\idx -> idx + 1)

wordLeftOffset : Document -> Cursor -> Int
wordLeftOffset document cursor =
  let startPositions = document |> wordStarts
  in  case List.reverse startPositions |> find (\x -> x < cursor) of
        Just pos -> pos - cursor
        Nothing -> 0

wordRightOffset : Document -> Cursor -> Int
wordRightOffset document cursor =
  let startPositions = document |> wordStarts
  in  case startPositions |> find (\x -> x > cursor) of
        Just pos -> pos - cursor
        Nothing -> 0

lineLeftOffset : Document -> Cursor -> Int
lineLeftOffset document cursor =
  let positions = 0 :: lineStarts document
  in  case List.reverse positions |> find (\x -> x <= cursor) of
        Just pos -> pos - cursor
        Nothing -> 0

lineRightOffset : Document -> Cursor -> Int
lineRightOffset document cursor =
  let positions = lineEnds document ++ [String.length document]
  in  case positions |> find (\x -> x >= cursor) of
        Just pos -> pos - cursor
        Nothing -> String.length document - cursor

stepCursorLeft : Bool -> Bool -> State -> State
stepCursorLeft ctrl shift ({document, cursor, selection} as state) =
  let dist = if ctrl then wordLeftOffset document cursor else -1
      cursor' = max 0 (cursor + dist)
  in { state | cursor <- cursor' } |> stepSelection shift

stepCursorRight : Bool -> Bool -> State -> State
stepCursorRight ctrl shift ({document, cursor, selection} as state) =
  let dist = if ctrl then wordRightOffset document cursor else 1
      cursor' = min (String.length document) (cursor + dist)
  in { state | cursor <- cursor' } |> stepSelection shift

-- todo: implement
stepCursorUp : Bool -> Bool -> State -> State
stepCursorUp ctrl shift = identity

-- todo: implement
stepCursorDown : Bool -> Bool -> State -> State
stepCursorDown ctrl shift = identity

stepCursorPos1 : Bool -> Bool -> State -> State
stepCursorPos1 ctrl shift ({document, cursor, selection} as state) =
  let dist = if ctrl then -cursor else lineLeftOffset document cursor
      cursor' = max 0 (cursor + dist)
  in { state | cursor <- cursor' } |> stepSelection shift

stepCursorEnd : Bool -> Bool -> State -> State
stepCursorEnd ctrl shift ({document, cursor, selection} as state) =
  let dist = if ctrl then (String.length document - cursor)
                     else lineRightOffset document cursor
      cursor' = min (String.length document) (cursor + dist)
  in { state | cursor <- cursor' } |> stepSelection shift

stepCursor : Bool -> Bool -> Keyboard.KeyCode -> State -> State
stepCursor ctrl shift key ({document, cursor} as state) =
  (case key of
     35 -> stepCursorEnd
     36 -> stepCursorPos1
     37 -> stepCursorLeft
     39 -> stepCursorRight
     38 -> stepCursorUp
     40 -> stepCursorDown
     otherwise -> (\ _ _ state -> state)
   ) ctrl shift state

isSelected : Selection -> Bool
isSelected (start, end) = start /= end

sortPair : (Int, Int) -> (Int, Int)
sortPair (a, b) = if a <= b then (a, b) else (b, a)

deleteSelection : State -> State
deleteSelection ({document, selection, cursor} as state) =
  let (start, end) = sortPair selection
      part1 = String.slice 0 start document
      part2 = String.slice end (String.length document) document
  in  { state | document <- String.append part1 part2
              , selection <- setBoth start
              , cursor <- start }

setCursorAndSelection : Cursor -> State -> State
setCursorAndSelection pos state =
  { state | cursor <- pos
          , selection <- setBoth pos }

stepBackspace : Bool -> Keyboard.KeyCode -> State -> State
stepBackspace ctrl key ({selection, cursor} as state) =
  case key of
    8 -> if isSelected selection
            then deleteSelection state
            else let (part1, part2) = splitAtCursor state
                 in  if String.length part1 > 0
                       then { state | document <- String.append
                                        (String.slice 0 -1 part1) part2
                            } |> setCursorAndSelection (cursor - 1)
                       else state
    otherwise -> state

stepDelete : Bool -> Keyboard.KeyCode -> State -> State
stepDelete ctrl key ({selection} as state) =
  case key of
    46 -> if isSelected selection
            then deleteSelection state
            else let (part1, part2) = splitAtCursor state
                 in  if String.length part2 > 0
                       then { state | document <- String.append part1 <|
                                        String.slice 1 (String.length part2)
                                                       part2 }
                       else state
    otherwise -> state

stepType : Bool -> Keyboard.KeyCode -> State -> State
stepType shift key state =
  case showKey shift key of
    Just c -> let state' = deleteSelection state
                  (part1, part2) = splitAtCursor state'
                  cursor' = state'.cursor + 1
              in  { state' | document <- String.concat
                               [ part1 , (String.fromList [c]) , part2 ]
                           , cursor <- cursor'
                           , selection <- setBoth cursor' }
    Nothing -> state

splitAtCursor : State -> (Document, Document)
splitAtCursor {document, cursor} =
  (String.slice 0 cursor document
  , String.slice cursor (String.length document) document)

applyWith : [a -> b -> b] -> a -> (b -> b)
applyWith fs x =
  let halfAppliedFs = map (\f -> f x) fs
  in  foldl (>>) identity halfAppliedFs

step : State -> Set.Set Keyboard.KeyCode -> [Keyboard.KeyCode] -> State
step ({document, cursor} as state) keysDown keysDownNew =
  let keysDownAll = Set.union keysDown (Set.fromList keysDownNew)
      shift = Set.member 16 keysDownAll
      ctrl = Set.member 17 keysDownAll
      stepKey = applyWith [
                  stepBackspace ctrl
                , stepDelete ctrl
                , if ctrl then (flip always) else stepType shift
                , stepCursor ctrl shift
                ]
      -- todo ctrl selection, delete/bs, steps and copypaste
  in  foldl stepKey state keysDownNew

getPos : Document -> Cursor -> (Int, Int)
getPos document position =
  let before = String.slice 0 position document
      above = List.take (List.length rows - 1) rows |> String.join "\n"
      lastRowX = position - String.length above
      rows = before |> String.lines
      y = List.length rows - 1
      x = if y == 0 then lastRowX else lastRowX - 1 -- todo: why? O_o
  in  (x, y)

-- todo: fix for browser font scaling
charSize : (Int, Int)
charSize = (widthOf <| displayText " ", heightOf (displayText "\n"))

displayCursor : Document -> Cursor -> Element
displayCursor document cursor =
  let (x, y) = getPos document cursor
      (charWidth, lineHeight) = charSize
      topSpacer = spacer 1 (lineHeight * y)
      leftSpacer = spacer (charWidth * x) 1
  in  flow right [ leftSpacer
                 , flow down [ topSpacer
                             , spacer 1 lineHeight |> color white1 ] ]

displaySelectionLine : Int -> (Int, Int) -> Element
displaySelectionLine y (x1, x2) =
  let (charWidth, lineHeight) = charSize
      topSpacer = spacer 1 (lineHeight * y)
      leftSpacer = spacer (charWidth * x1) 1
      spacerWidth = (x2 - x1) * charWidth
  in  flow right [ leftSpacer
                 , flow down [ topSpacer
                             , spacer spacerWidth lineHeight
                               |> color darkGray1 ] ]

safeHead : a -> [a] -> a
safeHead def xs = if List.isEmpty xs then def else head xs

displayTwoLineSelection : (Int, (Int, Int)) -> (Int, Int) -> Element
displayTwoLineSelection (startY, (startX, line1Length)) (endY, endX) =
  [
    displaySelectionLine startY (startX, line1Length)
  , displaySelectionLine endY (0, endX)
  ] |> flow outward

displaySelection : Document -> Selection -> Element
displaySelection document selection =
  let (start, end) = sortPair selection
      rows = String.lines document
      (startX, startY) = getPos document start
      (endX, endY) = getPos document end
      lineCnt = 1 + endY - startY
      line1Length = rows |> List.drop startY |> safeHead "" |> String.length
  in  if | lineCnt == 1 -> displaySelectionLine startY (startX, endX)
         | lineCnt == 2 -> displayTwoLineSelection
                            (startY, (startX, line1Length))
                            (endY, endX)
         | lineCnt >= 3 ->
              let middleYs = [ startY + 1 .. endY - 1 ]
                  middleLines = rows |> drop (startY + 1)
                                     |> take (lineCnt - 2)
                  middleLineLenghts = map String.length middleLines
                  middleLineRanges = map (\x -> (0, x)) middleLineLenghts
              in  zipWith displaySelectionLine middleYs middleLineRanges
                    ++ [displayTwoLineSelection
                              (startY, (startX, line1Length))
                              (endY, endX)]
                    |> flow outward
         | otherwise -> empty



displayText : Document -> Element
displayText =
     Text.toText
  >> Text.typeface ["inconsolata", "courier new", "monospace"]
  >> Text.height 20
  >> Text.color white1
  >> Text.leftAligned

display : State -> Element
display ({document, cursor, selection} as state) =
  flow outward [
                 if isSelected selection
                    then displaySelection document selection
                    else empty
               , displayCursor document cursor
               , displayText document
               ]