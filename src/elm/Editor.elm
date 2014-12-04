module Editor where

import Keyboard
import Set

type State = {document:String}

initialState : State
initialState = State ""

step : State -> Set.Set Int -> Set.Set Int -> State
step ({document} as state) keysDown keysDownNew =
  let document' = if isEmpty (Set.toList keysDownNew)
                     then document
                     else document ++ show (Set.toList keysDownNew)
  in  { state | document <- document' }

display : State -> Element
display {document} = plainText document