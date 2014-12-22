module KeyHistory where

import Keyboard
import Set
import Time
import Dict
import List
import Char(fromCode)
import String(cons)
import Graphics.Element (Element, spacer, color, flow, right)

import Layout (toDefText, toColText, white1, lightGray1, darkGray1, gray1
  , toSizedText)

type alias KeyAction = (Int, Keyboard.KeyCode)

keyActionTime : KeyAction -> Int
keyActionTime = fst

showKeyAction : KeyAction -> Element
showKeyAction (_, key) = showKey key |> toColText white1

type alias State = { history : List KeyAction }

initialState : State
initialState = State []

keyStrings : Dict.Dict Keyboard.KeyCode String
keyStrings =
  let strFromCode = fromCode >> (flip cons) ""
  in  [
        (37, "left")
      , (38, "up")
      , (39, "right")
      , (40, "down")
      , (16, "shift")
      , (17, "ctrl")
      , (32, "space")
      , (13, "enter")
      , (35, "end")
      , (36, "pos1")
      , (45, "insert")
      , (188, ",")
      , (190, ".")
      , (46, "del")
      , (8, "bs")
      , (9, "tab")
      ]
      ++ (List.map2 (,) [48..57] (List.map strFromCode [48..57])) -- top numbers
      ++ (List.map2 (,) [96..105] (List.map strFromCode [48..57])) -- block numbers
      ++ (List.map2 (,) [65..90] (List.map strFromCode [97..122])) -- a to z
      |> Dict.fromList

showKey : Keyboard.KeyCode -> String
showKey key = case Dict.get key keyStrings of
  Just result -> result
  Nothing -> ""

step : State -> Set.Set Keyboard.KeyCode -> List Keyboard.KeyCode
             -> List Keyboard.KeyCode -> Int -> State
step ({history} as state) keysDown keysDownNew keysUpNew time =
  let history' = history ++ (keysDownNew |> List.map (\x -> (time, x)))
  in  { state | history <- history' }

last : List a -> a
last xs = List.drop (List.length xs - 1) xs |> List.head

getTimeSpan : State -> Int
getTimeSpan {history} =
  if List.length history < 2
    then 0
    else (history |> last |> keyActionTime) -
         (history |> List.head |> keyActionTime)

getEndTime : State -> Int
getEndTime {history} = history |> last |> keyActionTime

display : Int -> State -> Element
display w {history} =
  let visibleHistory = List.drop (List.length history - 8) history
      sep = flow right [ spacer 2 1, spacer 2 18 |> color gray1, spacer 2 1 ]
  in  List.map showKeyAction visibleHistory
      |> List.intersperse sep
      |> flow right

getKeyCount : State -> Int
getKeyCount {history} = history |> List.length