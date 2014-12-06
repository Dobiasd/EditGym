module Main where

import Keyboard
import Set
import Window

import Editor
import Skeleton (showPage)

type State = { editor : Editor.State, keysDown : Set.Set Int }
type Input = { inKeysDown : Set.Set Int }

input : Signal Input
input = (Input << Set.fromList << filter validKey) <~ Keyboard.keysDown

validKey : Int -> Bool
validKey key =
  if | key >= 65 && key <= 90 -> True -- a to z
     | key >= 37 && key <= 40 -> True -- arrow keys
     | key == 16 -> True -- shift
     | key == 17 -> True -- ctrl
     | key == 32 -> True -- space
     | key >= 48 && key <= 57 -> True -- top numbers
     | key >= 96 && key <= 105 -> True -- block numbers
     | key == 13 -> True -- enter
     | key == 33 || key == 34 -> True -- page up, page down
     | key == 35 || key == 36 -> True -- end, pos1
     | key == 106 || key == 107 || key == 109 -> True -- *, +, -
     | key == 188 || key == 190 -> True -- , and .
     | key == 46 -> True -- delete
     | key == 8 -> True -- backspace
     | otherwise -> True

initialState : State
initialState = State Editor.initialState Set.empty

state : Signal State
state = foldp step initialState input

step : Input -> State -> State
step {inKeysDown} ({editor, keysDown} as state) =
  let keysDownNew = Set.diff inKeysDown keysDown
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keysDown <- inKeysDown }

main : Signal Element
main = scene <~ Window.width ~ state

scene : Int -> State -> Element
scene w {editor} =
  let content = Editor.display editor
  in  showPage w content