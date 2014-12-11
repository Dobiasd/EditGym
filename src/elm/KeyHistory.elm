module KeyHistory where

import Keyboard
import Set
import Dict
import Char(fromCode)
import String(cons)

import Layout (toDefText, toColText, white1, lightGray1, darkGray1)

data KeyAction = Up Int | Down Int

showKeyAction : KeyAction -> Element
showKeyAction action = case action of
  Up key   -> showKey key |> toColText lightGray1
  Down key -> showKey key |> toColText white1

type State = {history:[KeyAction]}

initialState : State
initialState = State []

keyStrings : Dict.Dict Int String
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
  , (106, "*")
  , (107, "+")
  , (109, "-")
  , (188, ",")
  , (190, "-")
  , (173, "-")
  , (46, "del")
  , (8, "bs")
  ]
  ++ (map (toDefStrPair identity) [48..57]) -- top numbers
  ++ (map (toDefStrPair identity) [96..105]) -- block numbers
  ++ (map (toDefStrPair (\x -> x - 32)) [97..122]) -- a to z
  |> Dict.fromList

-- todo: use String.fromChar
showKey : Int -> String
showKey key = case Dict.get key keyStrings of
  Just result -> result
  Nothing -> ""
  --Nothing -> key |> fromCode |> (flip cons) ""

step : State -> Set.Set Int -> Set.Set Int -> Set.Set Int -> State
step ({history} as state) keysDown keysDownNew keysUpNew =
  let history' = history ++ (Set.toList keysDownNew |> map Down)
                         ++ (Set.toList keysUpNew   |> map Up)
  in  { state | history <- history' }

display : State -> Element
display {history} =
  let visibleHistory = drop (length history - 30) history
      sep = spacer 2 20 |> color darkGray1
  in  map showKeyAction visibleHistory |> intersperse sep |> flow right