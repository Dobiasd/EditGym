module Game where

import Window
import Set
import Keyboard
import Time
import Text
import Signal
import List
import String
import Graphics.Element (Element, flow, down, right, outward, spacer, empty)

import Layout (toDefText, toSizedText, lightGray1, blue1, toColText)
import Skeleton
import Editor
import KeyHistory

type alias State = {
    editor : Editor.State
  , keyHistory : KeyHistory.State
  , keysDown : Set.Set Keyboard.KeyCode
  , goal : String
  , coach : String
  , timeInMs : Int
  }

port start : Signal String
port goal : Signal String
port coach : Signal String

initialState : State
initialState = State Editor.initialState
                     KeyHistory.initialState
                     Set.empty
                     "loading"
                     "loading"
                     0

type Input = Decisecond | Start String
                        | Goal String
                        | Coach String
                        | Keys Int (Set.Set Keyboard.KeyCode)

startInput : Signal Input
startInput = Signal.map Start start

goalInput : Signal Input
goalInput = Signal.map Goal goal

coachInput : Signal Input
coachInput = Signal.map Coach coach

keyInput : Signal Input
keyInput =
  Keyboard.keysDown
  |> Signal.map (Set.fromList << List.filter validKey)
  |> Time.timestamp
  |> Signal.map ((\(t, k) -> (floor t, k)) >> uncurry Keys)

-- https://groups.google.com/forum/#!topic/elm-discuss/Yo0KXZPLK9o
timeInput : Signal Input
timeInput = Signal.map (always Decisecond)
              <| Time.every (100 * Time.millisecond)

input : Signal Input
input = Signal.mergeMany [
    startInput
  , goalInput
  , keyInput
  , timeInput
  , coachInput
  ]

state : Signal State
state = Signal.foldp step initialState input

step : Input -> State -> State
step input =
  case input of
    Start start -> setStart start
    Goal goal -> setGoal goal
    Coach coach -> setCoach coach
    Keys time keys -> stepKeys keys time
    Decisecond -> stepDecisecond

stepDecisecond : State -> State
stepDecisecond ({editor, timeInMs, keyHistory, goal} as state) =
  if editor.document == goal || List.isEmpty keyHistory.history
    then state
    else { state | timeInMs <- timeInMs + 100 }

setStart : String -> State -> State
setStart start ({editor} as state) =
  { state | editor <- Editor.setDocument start editor }

setGoal : String -> State -> State
setGoal goal state = { state | goal <- goal }

setCoach : String -> State -> State
setCoach coach state = { state | coach <- coach }

stepKeys : Set.Set Keyboard.KeyCode -> Int -> State -> State
stepKeys inKeysDown time ({editor, keyHistory, keysDown, goal} as state) =
  let finished = editor.document == goal
      keysDownNew = Set.diff inKeysDown keysDown |> Set.toList
      keysUpNew = Set.diff keysDown inKeysDown |> Set.toList
  in  if finished then state
      else { state | editor <- Editor.step editor keysDown keysDownNew
                   , keyHistory <- KeyHistory.step keyHistory keysDown
                                                   keysDownNew keysUpNew
                                                   time
                   , keysDown <- inKeysDown }

main : Signal Element
main = Signal.map3 scene Window.width Window.height state

validKey : Keyboard.KeyCode -> Bool
validKey key = KeyHistory.showKey key /= ""

displayGoal : String -> Element
displayGoal goal = Editor.displayDocument lightGray1 goal

displayCoach : String -> Element
displayCoach str =
  let textElem = str
        |> Text.fromString
        |> Text.height 18
        |> Text.color blue1
        |> Text.leftAligned
  in  flow right [
          toColText blue1 "Coach: "
        , textElem
      ]

scene : Int -> Int -> State -> Element
scene w h ({editor, keyHistory, editor, goal, timeInMs, coach} as state) =
  let finished = editor.document == goal
      result = if finished
                 then flow down [
                     List.length keyHistory.history |> toString
                       |> toSizedText 180
                   , spacer 1 10
                   , KeyHistory.getTimeSpan keyHistory |> showTimeInMs
                     |> toSizedText 180
                   ]
                 else empty
      content = flow outward [
                    scenePlay w h state
                  , result
                  ]
  in  flow down [
          content
        , displayCoach coach
      ] |> Skeleton.showPage w h

showTimeInPrec : Int -> Int -> String
showTimeInPrec decimals timeInMs =
  let str = timeInMs |> toString |> String.dropRight (3 - decimals)
  in  String.concat [
           if timeInMs < 1000 then "0" else ""
        ,  String.dropRight decimals str
        , "."
        , String.right decimals str
        , "s" ]

showTimeInDs : Int -> String
showTimeInDs = showTimeInPrec 1

showTimeInMs : Int -> String
showTimeInMs = showTimeInPrec 3

-- todo: fixed widths, center texts
scenePlay : Int -> Int -> State -> Element
scenePlay w h {editor, keyHistory, goal, timeInMs} =
  let finished = editor.document == goal
  in  flow down [
          spacer 800 1
        , flow right [
              KeyHistory.display keyHistory
            , spacer 200 1
            , (if finished
                then KeyHistory.getTimeSpan keyHistory |> showTimeInMs
                else timeInMs |> showTimeInDs)
              |> toSizedText 48
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