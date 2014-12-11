module KeyHistory where

import Keyboard
import Set
import Char(fromCode)
import String(cons)

import Layout (toDefText)

type State = {history:[Int]}

initialState : State
initialState = State []

-- todo: use String.fromChar
showKey : Int -> String
showKey key = key |> fromCode |> (flip cons) ""

step : State -> Set.Set Int -> Set.Set Int -> State
step ({history} as state) keysDown keysDownNew =
  let history' = history ++ (Set.toList keysDownNew) -- todo: key combinations
  in  { state | history <- history' }

display : State -> Element
display {history} =
  let visibleHistory = drop (length history - 5) history
  in  show (map showKey visibleHistory) |> toDefText