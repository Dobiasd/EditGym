module Editor where

import Keyboard
import Set
import String
import Dict
import Maybe
import List
import Text
import Char(fromCode)

import Layout (toDefText, white1)

type Document = String
type Cursor = Int
type Selection = Maybe (Int, Int)

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
initialState = State "" 0 Nothing

stepCursorLeft : Document -> Cursor -> Cursor
stepCursorLeft _ cursor =
  if cursor > 0
    then cursor - 1
    else cursor

stepCursorRight : Document -> Cursor -> Cursor
stepCursorRight document cursor =
  if cursor < (String.length document)
    then cursor + 1
    else cursor

-- todo: implement
stepCursorUp : Document -> Cursor -> Cursor
stepCursorUp _ = identity

-- todo: implement
stepCursorDown : Document -> Cursor -> Cursor
stepCursorDown _ = identity

-- todo: implement
stepCursorPos1 : Document -> Cursor -> Cursor
stepCursorPos1 _ = identity

-- todo: implement
stepCursorEnd : Document -> Cursor -> Cursor
stepCursorEnd _ = identity

stepCursor : Document -> Keyboard.KeyCode -> Cursor -> Cursor
stepCursor document key cursor =
  case key of
    35 -> stepCursorEnd document cursor
    36 -> stepCursorPos1 document cursor
    37 -> stepCursorLeft document cursor
    38 -> stepCursorUp document cursor
    39 -> stepCursorRight document cursor
    40 -> stepCursorDown document cursor
    otherwise -> cursor

stepBackspace : Keyboard.KeyCode -> Document -> Document
stepBackspace key part1 =
  case key of
    8 -> if String.length part1 > 0
           then String.slice 0 -1 part1
           else part1
    otherwise -> part1

stepDelete : Keyboard.KeyCode -> Document -> Document
stepDelete key part2 =
  case key of
    46 -> if String.length part2 > 0
            then String.slice 1 (String.length part2) part2
            else part2
    otherwise -> part2

stepType : Bool -> Keyboard.KeyCode -> Document -> Document
stepType shift key part1 =
  case showKey shift key of
    Just c -> String.append part1 (String.fromList [c])
    Nothing -> part1

step : State -> Set.Set Keyboard.KeyCode -> [Keyboard.KeyCode] -> State
step ({document, cursor, selection} as state) keysDown keysDownNew =
  let part1 = String.slice 0 cursor document
      part2 = String.slice cursor (String.length document) document
      keysDownAll = Set.union keysDown (Set.fromList keysDownNew)
      shift = Set.member 16 keysDownAll
      part1' = foldl stepBackspace part1 keysDownNew
               |> flip (foldl (stepType shift)) keysDownNew
      part2' = foldl stepDelete part2 keysDownNew
      document' = String.append part1' part2'
      newChars = List.filterMap (showKey False) keysDownNew
      cursor' = String.length part1'
  in { state | document <- document'
             , cursor <- foldl (stepCursor document) cursor' keysDownNew }

display : State -> Element
display {document, cursor, selection} =
  flow down [
    document
      |> Text.toText
      |> Text.typeface ["inconsolata", "courier new", "monospace"]
      |> Text.height 20
      |> Text.color white1
      |> Text.leftAligned
  , cursor |> show |> toDefText
  , selection |> show |> toDefText
  ]