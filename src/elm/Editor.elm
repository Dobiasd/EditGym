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

stepCursorLeft : Bool -> State -> State
stepCursorLeft shift ({cursor, selection} as state) =
  let cursor' = max 0 (cursor - 1)
  in { state | cursor <- cursor' } |> stepSelection shift

stepCursorRight : Bool -> State -> State
stepCursorRight shift ({document, cursor, selection} as state) =
  let cursor' = min (String.length document) (cursor + 1)
  in { state | cursor <- cursor' } |> stepSelection shift

-- todo: implement
stepCursorUp : Bool -> State -> State
stepCursorUp _ = identity

-- todo: implement
stepCursorDown : Bool -> State -> State
stepCursorDown _ = identity

-- todo: implement
stepCursorPos1 : Bool -> State -> State
stepCursorPos1 _ = identity

-- todo: implement
stepCursorEnd : Bool -> State -> State
stepCursorEnd _ = identity

stepCursor : Bool -> Bool -> Keyboard.KeyCode -> State -> State
stepCursor ctrl shift key ({document, cursor} as state) =
  (case key of
     35 -> stepCursorEnd
     36 -> stepCursorPos1
     37 -> stepCursorLeft
     39 -> stepCursorRight
     38 -> stepCursorUp
     40 -> stepCursorDown
     otherwise -> (flip always)
   ) shift state

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

stepBackspace : Bool -> Keyboard.KeyCode -> State -> State
stepBackspace ctrl key ({selection} as state) =
  case key of
    8 -> if isSelected selection
            then deleteSelection state
            else let (part1, part2) = splitAtCursor state
                 in  if String.length part1 > 0
                       then { state | document <- String.append
                                        (String.slice 0 -1 part1) part2
                                    , cursor <- state.cursor - 1 }
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
      -- todo ctrl selection, steps and copypaste
  in  foldl stepKey state keysDownNew

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