module KeyHistory where

import Keyboard
import Set
import Dict
import List
import Char(fromCode)
import String(cons)
import Graphics.Element (Element, spacer, color, flow, right)

import Layout (toDefText, toColText, white1, lightGray1, darkGray1)

type KeyAction = Up Keyboard.KeyCode | Down Keyboard.KeyCode

showKeyAction : KeyAction -> Element
showKeyAction action = case action of
  Up key   -> showKey key |> toColText lightGray1
  Down key -> showKey key |> toColText white1

type alias State = { history : List KeyAction }

initialState : State
initialState = State []

keyStrings : Dict.Dict Keyboard.KeyCode String
keyStrings =
  let toDefStrPair mod key = (mod key, key |> fromCode |> (flip cons) "")
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
  ]
  ++ (List.map (toDefStrPair identity) [48..57]) -- top numbers
  ++ (List.map (toDefStrPair identity) [96..105]) -- block numbers
  ++ (List.map (toDefStrPair (\x -> x - 32)) [97..122]) -- a to z
  |> Dict.fromList

showKey : Keyboard.KeyCode -> String
showKey key = case Dict.get key keyStrings of
  Just result -> result
  Nothing -> ""

step : State -> Set.Set Keyboard.KeyCode -> List Keyboard.KeyCode
             -> List Keyboard.KeyCode -> State
step ({history} as state) keysDown keysDownNew keysUpNew =
  let history' = history ++ (keysDownNew |> List.map Down)
                         ++ (keysUpNew   |> List.map Up)
  in  { state | history <- history' }

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