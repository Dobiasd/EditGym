module Game where

import Window
import Set
import Keyboard
import Time
import Text
import Signal
import List
import String
import Regex
import Graphics.Element (Element, flow, down, right, outward, spacer, empty
  , heightOf, color, widthOf, link)

import Stars (keyStarsElem)
import Layout (toDefText, toSizedText, lightGray1, blue1, toColText
  , quadDefSpacer, toColoredSizedText, orange1, centerHorizontally, gray1
  , showRightBottom, defaultSpacer, green1, octaDefSpacer, defTextSize)
import Skeleton
import Editor
import KeyHistory
import ExercisesList
import PersonalBests
import DateTime(timeToString)

type alias State = {
    editor : Editor.State
  , keyHistory : KeyHistory.State
  , keysDown : Set.Set Keyboard.KeyCode
  , start : String
  , goal : String
  , coach : String
  , timeInMs : Int
  , prev : (String, String)
  , exercise : (String, String)
  , next : (String, String)
  , redirectTo : String
  , waitForNoKeys : Bool
  , personalBests : PersonalBests.PBs
  , savedPB : Bool
  }

port startIn : Signal String
port goalIn : Signal String
port coachIn : Signal String
port exerciseIn : Signal String
port loadPBsIn : Signal String

port savePBsOut : Signal String
port savePBsOut =
  state
  |> Signal.keepIf .savedPB initialState
  |> Signal.map .personalBests
  |> Signal.dropRepeats
  |> Signal.map PersonalBests.showBests

port redirect : Signal String
port redirect = Signal.map .redirectTo state

regExReplaceInStr : String -> String -> String -> String
regExReplaceInStr exp rep =
  Regex.replace Regex.All (Regex.regex exp) (\_ -> rep)

cleanEditorText  : String -> String
cleanEditorText =
  cleanCoachText
  >> regExReplaceInStr "[^0-9A-z ,.\\n]" ""

cleanCoachText : String -> String
cleanCoachText =
  regExReplaceInStr "\\r\\n" "\n"
  >> regExReplaceInStr "\\r" "\n"
  >> regExReplaceInStr "\\t" "    "
  >> regExReplaceInStr "[^0-9A-z ,.'!\\]+\\-*/\\(\\)\"\\n:]" ""

initialState : State
initialState = State Editor.initialState
                     KeyHistory.initialState
                     Set.empty
                     "loading"
                     "loading"
                     "loading"
                     0
                     ("", "")
                     ("", "")
                     ("", "")
                     ""
                     False
                     []
                     False

type Input = Decisecond | Start String
                        | Goal String
                        | Coach String
                        | Exercise String
                        | PBs String
                        | Keys Int (Set.Set Keyboard.KeyCode)

startInput : Signal Input
startInput = Signal.map (cleanEditorText >> Start) startIn

goalInput : Signal Input
goalInput = Signal.map (cleanEditorText >> Goal) goalIn

coachInput : Signal Input
coachInput = Signal.map (cleanCoachText >> Coach) coachIn

loadPBsInput : Signal Input
loadPBsInput = Signal.map PBs loadPBsIn

exerciseInput : Signal Input
exerciseInput = Signal.map Exercise exerciseIn

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
  , loadPBsInput
  , exerciseInput
  ]

state : Signal State
state = Signal.foldp step initialState input

step : Input -> State -> State
step input =
  case input of
    Start start -> setStart start
    Goal goal -> setGoal goal
    Coach coach -> setCoach coach
    Exercise exercise -> setExercise exercise
    Keys time keys -> stepKeys keys time
    Decisecond -> stepDecisecond
    PBs pbs -> setPersonalBests pbs

stepDecisecond : State -> State
stepDecisecond ({editor, timeInMs, keyHistory, goal} as state) =
  if editor.document == goal || List.isEmpty keyHistory.history
    then state
    else { state | timeInMs <- timeInMs + 100 }

setStart : String -> State -> State
setStart start ({editor} as state) =
  { state | editor <- Editor.setDocument start editor
          , start <- start}

setGoal : String -> State -> State
setGoal goal state = { state | goal <- goal }

setCoach : String -> State -> State
setCoach coach state = { state | coach <- coach }

setPersonalBests : String -> State -> State
setPersonalBests pbs state =
  { state | personalBests <- PersonalBests.readBests pbs }

setExercise : String -> State -> State
setExercise exercise state =
  let prev = ExercisesList.getPrev exercise
      next = ExercisesList.getNext exercise
  in  { state | exercise <- (exercise, ExercisesList.getCategorie exercise)
              , prev <- (prev, ExercisesList.getCategorie prev)
              , next <- (next, ExercisesList.getCategorie next) }

stepSwitchExercise : Set.Set Keyboard.KeyCode -> State -> State
stepSwitchExercise keys ({prev, next, start, personalBests} as state) =
  let space = Set.member 32 keys
      p = Set.member 80 keys
      r = Set.member 82 keys
      n = Set.member 78 keys
  in  if | space && r -> { state | editor <- (Editor.setDocument
                                               start
                                               Editor.initialState)
                                , keyHistory <- KeyHistory.initialState
                                , timeInMs <- 0
                                , waitForNoKeys <- True
                                , personalBests <- personalBests }
         | space && p -> { state | redirectTo <- exerciseLink (fst prev) }
         | space && n -> { state | redirectTo <- exerciseLink (fst next) }
         | otherwise -> state

stepKeysEdit : Set.Set Keyboard.KeyCode -> Int -> State -> State
stepKeysEdit inKeysDown time
             ({editor, keyHistory, keysDown, goal} as state) =
  let keysDownNew = Set.diff inKeysDown keysDown |> Set.toList
      keysUpNew = Set.diff keysDown inKeysDown |> Set.toList
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keyHistory <- KeyHistory.step keyHistory keysDown
                                              keysDownNew keysUpNew
                                              time
              , keysDown <- inKeysDown }

stepKeys : Set.Set Keyboard.KeyCode -> Int -> State -> State
stepKeys inKeysDown time
         ({editor, keyHistory, keysDown, goal, waitForNoKeys
         , personalBests, savedPB, exercise} as state) =
  let finished = editor.document == goal
      (savedPB', personalBests') =
        if finished && (not savedPB)
           then let endTime = KeyHistory.getEndTime keyHistory
                    dateStr = timeToString endTime
                    newPBs = PersonalBests.insert personalBests
                               { name = fst exercise
                               , keys = List.length keyHistory.history
                               , time = KeyHistory.getTimeSpan keyHistory
                               , keysdate = dateStr
                               , timedate = dateStr }
                in  (newPBs /= personalBests, newPBs)
           else (True, personalBests)
      state' = { state | savedPB <- savedPB'
                       , personalBests <- personalBests' }
      state'' =
        if | finished -> state'
           | state.waitForNoKeys && (not <| inKeysDown == Set.empty) -> state
           | state.waitForNoKeys && inKeysDown == Set.empty ->
             { state | waitForNoKeys <- False
                     , keysDown <- Set.empty }
           | not <| waitForNoKeys -> stepKeysEdit inKeysDown time state
           | otherwise -> state
  in  state'' |> stepSwitchExercise inKeysDown

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
        |> Text.height 20
        |> Text.color blue1
        |> Text.leftAligned
  in  flow right [
          toColText blue1 "Coach: "
        , textElem
      ]


showHeadline : String -> Element
showHeadline = toColoredSizedText green1 32


scene : Int -> Int -> State -> Element
scene w h
  ({editor, keyHistory, editor, goal, timeInMs, coach, exercise} as state) =
  let header = String.concat [snd exercise, " - ", fst exercise]
      headline = flow down [ defaultSpacer, showHeadline header ]
  in  scenePlay w h state |> Skeleton.showPageWithHeadline w h headline

showTimeInPrec : Int -> Int -> String
showTimeInPrec decimals timeInMs =
  let str = timeInMs |> toString |> String.dropRight (3 - decimals)
      part2 = String.right decimals str
  in  String.concat [
           if timeInMs < 1000 then "0" else ""
        ,  String.dropRight decimals str
        , "."
        , if String.isEmpty part2 then "0" else part2
        , "s" ]

showTimeInDs : Int -> String
showTimeInDs = showTimeInPrec 1

showTimeInMs : Int -> String
showTimeInMs = showTimeInPrec 3

showPressedKeys : Set.Set Keyboard.KeyCode -> Element
showPressedKeys =
  Set.toList
  >> List.map KeyHistory.showKey
  >> String.join ","
  >> toDefText

showExercise : (String, String) -> (Text.Text -> Element) -> Element
showExercise (name, cat) align =
  String.concat ["\n", name, "\n(", cat, ")"]
  |> Text.fromString
     >> Text.height defTextSize
     >> Text.color green1
     >> align

exerciseLink : String -> String
exerciseLink str =
  if String.isEmpty str
     then ""
     else String.append "?page=game&exercise=" str

toExerciseLink : String -> (Element -> Element)
toExerciseLink = exerciseLink >> link

showPrev : (String, String) -> Element
showPrev (name, cat) =
  if String.isEmpty name then empty else
    flow right [
        showExercise (name, cat) Text.rightAligned
      , toColoredSizedText green1 82 " <-- "
      , toColText green1 "\n(space+p)"
    ] |> toExerciseLink name

showNext : (String, String) -> Element
showNext (name, cat) =
  if String.isEmpty name then empty else
    flow right [
        toColText green1 "\n(space+n)"
      , toColoredSizedText green1 82 " --> "
      , showExercise (name, cat) Text.leftAligned
    ] |> toExerciseLink name

showRestart : Element
showRestart =
  "\nrestart\n(space+r)"
  |> Text.fromString
     >> Text.height defTextSize
     >> Text.color green1
     >> Text.centered

showButtons : Int -> State -> Element
showButtons w {exercise, prev, next} =
  flow outward [
      showPrev prev
    , showRestart |> centerHorizontally w
    , showNext next |> showRightBottom w
  ]

coachResult : State -> List Element
coachResult {keyHistory, exercise, next} =
  let keyMoves = List.length keyHistory.history
      span = KeyHistory.getTimeSpan keyHistory
      name = fst exercise
      text =  String.concat [
                  "You needed "
                , keyMoves |> toString
                , " key moves and "
                , span |> showTimeInMs |> String.dropRight 1
                , " seconds."
                , if String.isEmpty (fst next)
                     then ""
                     else "\nYou can go on to the next exercise (space+n) or try to improve (space+r)."
              ]
  in  [ displayCoach text
      , keyStarsElem False 1 name keyMoves span
      ]

scenePlay : Int -> Int -> State -> Element
scenePlay winW winH
  ({editor, keyHistory, goal, timeInMs, keysDown, coach} as state) =
  let finished = editor.document == goal
      editorElem = flow down [
                       spacer 560 1 |> color gray1
                     , spacer 1 3
                     , "Your editor" |> toColoredSizedText orange1 28
                                     |> centerHorizontally 560
                     , spacer 1 10
                     , Editor.display editor
                   ]
      goalElem = flow down [
                     spacer 560 1 |> color gray1
                   , spacer 1 3
                   , "Goal" |> toColoredSizedText orange1 28
                            |> centerHorizontally 560
                   , spacer 1 10
                   , displayGoal goal
                 ]
      middleElem = flow right [
                       editorElem
                     , spacer 14 1
                     , spacer 1 (max (heightOf editorElem) (heightOf goalElem))
                       |> color gray1
                     , spacer 14 1
                     , goalElem
                   ]
      timeElem = (if finished
                     then KeyHistory.getTimeSpan keyHistory |> showTimeInMs
                     else timeInMs |> showTimeInDs)
                 |> toSizedText 32
      pressedKeysElem = showPressedKeys keysDown
      w = widthOf middleElem
      verticalScalerH = max 0 (230 - heightOf middleElem)
      coachElems = if finished then coachResult state
                               else [displayCoach coach]
  in  flow down [
          flow outward [
              KeyHistory.display 560 keyHistory
            , timeElem |> centerHorizontally w
            , showRightBottom w pressedKeysElem
          ]
        , spacer 1 3
        , middleElem
        , spacer 1 verticalScalerH
        , defaultSpacer
        , coachElems |> List.map (centerHorizontally w) |> flow down
        , showButtons w state
      ]