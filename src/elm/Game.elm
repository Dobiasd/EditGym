module Game where

import Window
import Set
import Keyboard
import Time
import List
import String

import Layout (toDefText, toSizedText, lightGray1)
import Skeleton
import Editor
import KeyHistory

type State = {
    editor : Editor.State
  , keyHistory : KeyHistory.State
  , keysDown : Set.Set Keyboard.KeyCode
  , goal : String
  , timeInDs : Int
  }

port start : Signal String
port goal : Signal String

initialState : State
initialState = State Editor.initialState
                     KeyHistory.initialState
                     Set.empty
                     "loading"
                     0

data Input = Decisecond | Start String
                        | Goal String
                        | Keys (Set.Set Keyboard.KeyCode)

startInput : Signal Input
startInput = Start <~ start

goalInput : Signal Input
goalInput = Goal <~ goal

keyInput : Signal Input
keyInput = Keys <~ ((Set.fromList << filter validKey) <~ Keyboard.keysDown)


{-| We want the time to update every 100 milliseconds if possible. -}
ticker = lift (\t -> t / 1000) <| fps 10

timeInput : Signal Input
timeInput = (always Decisecond) <~ every (100 * millisecond)

input : Signal Input
input = merges [startInput, goalInput, keyInput, timeInput]

state : Signal State
state = foldp step initialState input

step : Input -> State -> State
step input =
  case input of
    Start start -> setStart start
    Goal goal -> setGoal goal
    Keys keys -> stepKeys keys
    Decisecond -> stepDecisecond

stepDecisecond : State -> State
stepDecisecond ({editor, timeInDs, keyHistory, goal} as state) =
  if editor.document == goal || List.isEmpty keyHistory.history
    then state
    else { state | timeInDs <- timeInDs + 1 }

setStart : String -> State -> State
setStart start ({editor} as state) =
  { state | editor <- Editor.setDocument start editor }

setGoal : String -> State -> State
setGoal goal state = { state | goal <- goal }

-- todo: check if start == goal, but not empty (on load errors)
stepKeys : Set.Set Keyboard.KeyCode -> State -> State
stepKeys inKeysDown ({editor, keyHistory, keysDown, goal} as state) =
  let finished = editor.document == goal
      keysDownNew = Set.diff inKeysDown keysDown |> Set.toList
      keysUpNew = Set.diff keysDown inKeysDown |> Set.toList
  in  if finished then state
      else { state | editor <- Editor.step editor keysDown keysDownNew
                   , keyHistory <- KeyHistory.step keyHistory keysDown
                                                   keysDownNew keysUpNew
                   , keysDown <- inKeysDown }

main : Signal Element
main = scene <~ Window.width ~ Window.height ~ state

validKey : Keyboard.KeyCode -> Bool
validKey key = KeyHistory.showKey key /= ""

displayGoal : String -> Element
displayGoal goal = Editor.displayTextCol lightGray1 goal

scene : Int -> Int -> State -> Element
scene w h ({editor, keyHistory, editor, goal, timeInDs} as state) =
  let finished = editor.document == goal
      result = if finished
                 then flow down [
                     length keyHistory.history |> show |> toSizedText 180
                   , spacer 1 10
                   , showTime timeInDs |> toSizedText 180
                   ]
                 else empty
      content = flow outward [
                    scenePlay w h state
                  , result
                  ]
  in  Skeleton.showPage w h content

showTime : Int -> String
showTime timeInDs =
  let str = toFloat timeInDs / 10 |> show
      str' = if (String.right 2 str |> String.left 1) == "."
               then str
               else String.append str ".0"
  in  String.append str' "s"

displayTime : Int -> Element
displayTime timeInDs =
   showTime timeInDs |> toSizedText 48

-- todo: fixed widths
scenePlay : Int -> Int -> State -> Element
scenePlay w h {editor, keyHistory, goal, timeInDs} =
  flow down [
      spacer 800 1
    , flow right [
          KeyHistory.display keyHistory
        , spacer 200 1
        , displayTime timeInDs
      ]
    , flow right [
          flow down [
              spacer 400 1
            , "editor" |> toSizedText 48
            , spacer 1 30
            , Editor.display editor
          ]
        , spacer 100 1
        , flow down [
              spacer 400 1
            , "goal" |> toSizedText 48
            , spacer 1 30
            , displayGoal goal
          ]
      ]
  ]