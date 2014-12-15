module Game where

import Window
import Set
import Keyboard
import Time
import Signal
import List
import String
import Graphics.Element (Element, flow, down, right, outward, spacer, empty)

import Layout (toDefText, toSizedText, lightGray1)
import Skeleton
import Editor
import KeyHistory

type alias State = {
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

type Input = Decisecond | Start String
                        | Goal String
                        | Keys (Set.Set Keyboard.KeyCode)

startInput : Signal Input
startInput = Signal.map Start start

goalInput : Signal Input
goalInput = Signal.map Goal goal

keyInput : Signal Input
keyInput = Signal.map Keys
                     (Signal.map (Set.fromList << List.filter validKey)
                                 Keyboard.keysDown)


{-| We want the time to update every 100 milliseconds if possible. -}
ticker = Signal.map (\t -> t / 1000) <| Time.fps 10

-- todo: do not discard time gap, use ticker
-- https://groups.google.com/forum/#!topic/elm-discuss/Yo0KXZPLK9o
timeInput : Signal Input
timeInput = Signal.map (always Decisecond)
              <| Time.every (100 * Time.millisecond)

input : Signal Input
input = Signal.mergeMany [startInput, goalInput, keyInput, timeInput]

state : Signal State
state = Signal.foldp step initialState input

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
main = Signal.map3 scene Window.width Window.height state

validKey : Keyboard.KeyCode -> Bool
validKey key = KeyHistory.showKey key /= ""

displayGoal : String -> Element
displayGoal goal = Editor.displayDocument lightGray1 goal

scene : Int -> Int -> State -> Element
scene w h ({editor, keyHistory, editor, goal, timeInDs} as state) =
  let finished = editor.document == goal
      result = if finished
                 then flow down [
                     List.length keyHistory.history |> toString
                       |> toSizedText 180
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
  let str = toFloat timeInDs / 10 |> toString
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