module Main where

import Keyboard
import Set
import Window

import Editor
import Skeleton (showPage)

type State = { editor : Editor.State, keysDown : Set.Set Int }
type Input = { inKeysDown : Set.Set Int }

input : Signal Input
input = (Input << Set.fromList) <~ Keyboard.keysDown

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