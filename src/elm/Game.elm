module Game where

import Window
import Set
import Keyboard

import Layout (toDefText)
import Skeleton
import Editor
import KeyHistory

type State = {
    editor : Editor.State
  , keyHistory : KeyHistory.State
  , keysDown : Set.Set Int
  }

port level : Signal String

initialState : State
initialState = State Editor.initialState KeyHistory.initialState Set.empty

type Input = { inKeysDown : Set.Set Int }

input : Signal Input
input = Input <~ ((Set.fromList << filter validKey) <~ Keyboard.keysDown)

state : Signal State
state = foldp step initialState input

step : Input -> State -> State
step {inKeysDown} ({editor, keyHistory, keysDown} as state) =
  let keysDownNew = Set.diff inKeysDown keysDown
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keyHistory <- KeyHistory.step keyHistory keysDown keysDownNew
              , keysDown <- inKeysDown }

main : Signal Element
main = scene <~ Window.width ~ Window.height ~ state

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
     -- | key == 33 || key == 34 -> True -- page up, page down
     | key == 35 || key == 36 -> True -- end, pos1
     | key == 106 || key == 107 || key == 109 -> True -- *, +, -
     | key == 188 || key == 190 -> True -- , and .
     | key == 46 -> True -- delete
     | key == 8 -> True -- backspace
     | otherwise -> True

scene : Int -> Int -> State -> Element
scene w h {editor, keyHistory} =
  let content = flow down [
                    KeyHistory.display keyHistory
                  , Editor.display editor
                ]
  in  Skeleton.showPage w h content