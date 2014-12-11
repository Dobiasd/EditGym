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
      keysUpNew = Set.diff keysDown inKeysDown
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keyHistory <- KeyHistory.step keyHistory keysDown
                                              keysDownNew keysUpNew
              , keysDown <- inKeysDown }

main : Signal Element
main = scene <~ Window.width ~ Window.height ~ state

validKey : Int -> Bool
validKey key = KeyHistory.showKey key /= ""

scene : Int -> Int -> State -> Element
scene w h {editor, keyHistory} =
  let content = flow down [
                    KeyHistory.display keyHistory
                  , Editor.display editor
                ]
  in  Skeleton.showPage w h content