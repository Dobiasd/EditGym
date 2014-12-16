module KeyHistory where

import Keyboard
import Set
import Time
import Dict
import List
import Char(fromCode)
import String(cons)
import Graphics.Element (Element, spacer, color, flow, right)

import Layout (toDefText, toColText, white1, lightGray1, darkGray1)

type KeyAction = Up Int Keyboard.KeyCode
               | Down Int Keyboard.KeyCode

keyActionTime : KeyAction -> Int
keyActionTime ka = case ka of
  Up time _ -> time
  Down time _ -> time

showKeyAction : KeyAction -> Element
showKeyAction action = case action of
  Up _ key   -> showKey key |> toColText lightGray1
  Down _ key -> showKey key |> toColText white1

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
  let history' = history ++ (keysDownNew |> List.map (Down time))
                         ++ (keysUpNew   |> List.map (Up time))
  in  { state | history <- history' }

last : List a -> a
last xs = List.drop (List.length xs - 1) xs |> List.head

getTimeSpan : State -> Int
getTimeSpan {history} =
  if List.length history < 2
    then 0
    else (history |> last |> keyActionTime) -
         (history |> List.head |> keyActionTime)

display : State -> Element
display {history} =
  let visibleHistory = List.drop (List.length history - 5) history
      sep = spacer 2 8 |> color darkGray1
  in  flow right [
        List.length history |> toString |> toDefText
      , spacer 30 1
      , List.map showKeyAction visibleHistory |> List.intersperse sep
        |> flow right
      ]