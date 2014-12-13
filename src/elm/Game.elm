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
  , keysDown : Set.Set Keyboard.KeyCode
  , goal : String
  }

port start : Signal String
port goal : Signal String

initialState : State
initialState = State Editor.initialState
                     KeyHistory.initialState
                     Set.empty
                     "loading"

data Input = Start String | Goal String | Keys (Set.Set Keyboard.KeyCode)

startInput : Signal Input
startInput = Start <~ start

goalInput : Signal Input
goalInput = Goal <~ goal

keyInput : Signal Input
keyInput = Keys <~ ((Set.fromList << filter validKey) <~ Keyboard.keysDown)

input : Signal Input
input = merges [startInput, goalInput, keyInput]

state : Signal State
state = foldp step initialState input

step : Input -> State -> State
step input =
  case input of
    Start start -> setStart start
    Goal goal -> setGoal goal
    Keys keys -> stepKeys keys

setStart : String -> State -> State
setStart start ({editor} as state) =
  { state | editor <- Editor.setDocument start editor }

setGoal : String -> State -> State
setGoal goal state = { state | goal <- goal }

stepKeys : Set.Set Keyboard.KeyCode -> State -> State
stepKeys inKeysDown ({editor, keyHistory, keysDown} as state) =
  let keysDownNew = Set.diff inKeysDown keysDown |> Set.toList
      keysUpNew = Set.diff keysDown inKeysDown |> Set.toList
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keyHistory <- KeyHistory.step keyHistory keysDown
                                              keysDownNew keysUpNew
              , keysDown <- inKeysDown }

main : Signal Element
main = scene <~ Window.width ~ Window.height ~ state

validKey : Keyboard.KeyCode -> Bool
validKey key = KeyHistory.showKey key /= ""

scene : Int -> Int -> State -> Element
scene w h {editor, keyHistory} =
  let content = flow down [
                    spacer 800 1
                  , KeyHistory.display keyHistory
                  , Editor.display editor
                ]
  in  Skeleton.showPage w h content