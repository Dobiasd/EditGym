module Editor where

import Keyboard
import Set
import Array

import Layout (toDefText)

type State = {document:String}

initialState : State
initialState = State ""

step : State -> Set.Set Keyboard.KeyCode -> Set.Set Keyboard.KeyCode -> State
step ({document} as state) keysDown keysDownNew =
  let document' = if isEmpty (Set.toList keysDownNew)
                     then document
                     else document ++ show (Set.toList keysDownNew)
  in  { state | document <- document' }

display : State -> Element
display {document} = toDefText document