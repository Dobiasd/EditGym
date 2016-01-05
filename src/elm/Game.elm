module Game (..) where

import Window
import Set
import Char
import Keyboard
import Time
import Text
import Signal
import List
import String
import Regex
import Graphics.Element exposing (Element, flow, down, right, outward, spacer, empty, heightOf, color, widthOf, link, rightAligned, leftAligned, centered)
import Stars exposing (starsElemFromPBs, fiveStarsInEverything, starsElem)
import Layout exposing (toDefText, toSizedText, lightGray1, blue1, toColText, quadDefSpacer, toColoredSizedText, orange1, centerHorizontally, gray1, showRightBottom, defaultSpacer, green1, octaDefSpacer, defTextSize, displayCoach, showTimeInDs, showTimeInMs)
import Skeleton
import Editor
import KeyHistory
import ExercisesList
import PersonalBests
import DateTime exposing (timeToString)


type alias State =
    { editor : Editor.State
    , keyHistory : KeyHistory.State
    , keysDown : Set.Set Char.KeyCode
    , start : String
    , goal : String
    , coach : String
    , timeInMs : Int
    , prev : ( String, String )
    , exercise : ( String, String )
    , next : ( String, String )
    , redirectTo : String
    , waitForNoKeys : Bool
    , personalBests : PersonalBests.PBs
    , finished : Bool
    , savedPB : Bool
    , improvedBestKeys : Bool
    , improvedBestTime : Bool
    , justGot5StarsEverywhere : Bool
    , thisTimePseudoPB : PersonalBests.PB
    }


port startIn : Signal String
port goalIn : Signal String
port coachIn : Signal String
port exerciseIn : Signal String
port loadPBsIn : Signal String
port savePBsOut : Signal String
port savePBsOut =
    state
        |> Signal.filter .savedPB initialState
        |> Signal.map .personalBests
        |> Signal.dropRepeats
        |> Signal.map PersonalBests.showBests


port redirect : Signal String
port redirect =
    Signal.dropRepeats <| Signal.map .redirectTo state


regExReplaceInStr : String -> String -> String -> String
regExReplaceInStr exp rep =
    Regex.replace Regex.All (Regex.regex exp) (\_ -> rep)


cleanEditorText : String -> String
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
initialState =
    State
        Editor.initialState
        KeyHistory.initialState
        Set.empty
        "loading"
        "loading"
        "loading"
        0
        ( "", "" )
        ( "", "" )
        ( "", "" )
        ""
        False
        []
        False
        False
        False
        False
        False
        (PersonalBests.PB "" 999999 "" 999999 "")


type Input
    = Decisecond
    | Start String
    | Goal String
    | Coach String
    | Exercise String
    | PBs String
    | Keys Int (Set.Set Char.KeyCode)


startInput : Signal Input
startInput =
    Signal.map (cleanEditorText >> Start) startIn


goalInput : Signal Input
goalInput =
    Signal.map (cleanEditorText >> Goal) goalIn


coachInput : Signal Input
coachInput =
    Signal.map (cleanCoachText >> Coach) coachIn


loadPBsInput : Signal Input
loadPBsInput =
    Signal.map PBs (Signal.dropRepeats loadPBsIn)


exerciseInput : Signal Input
exerciseInput =
    Signal.map Exercise exerciseIn


keyInput : Signal Input
keyInput =
    Keyboard.keysDown
        |> Signal.map (Set.filter validKey)
        |> Time.timestamp
        |> Signal.map ((\( t, k ) -> ( floor t, k )) >> uncurry Keys)



-- https://groups.google.com/forum/#!topic/elm-discuss/Yo0KXZPLK9o


timeInput : Signal Input
timeInput =
    Signal.map (always Decisecond)
        <| Time.every (100 * Time.millisecond)


input : Signal Input
input =
    Signal.mergeMany
        [ startInput
        , goalInput
        , keyInput
        , timeInput
        , coachInput
        , loadPBsInput
        , exerciseInput
        ]


state : Signal State
state =
    Signal.foldp step initialState input


step : Input -> State -> State
step input =
    case input of
        Start start ->
            setStart start

        Goal goal ->
            setGoal goal

        Coach coach ->
            setCoach coach

        Exercise exercise ->
            setExercise exercise

        Keys time keys ->
            stepKeys keys time

        Decisecond ->
            stepDecisecond

        PBs pbs ->
            setPersonalBests pbs


stepDecisecond : State -> State
stepDecisecond ({ editor, timeInMs, keyHistory, goal } as state) =
    if editor.document == goal || List.isEmpty keyHistory.history then
        state
    else
        { state | timeInMs = timeInMs + 100 }


setStart : String -> State -> State
setStart start ({ editor } as state) =
    { state
        | editor = Editor.setDocument start editor
        , start = start
    }


setGoal : String -> State -> State
setGoal goal state =
    { state | goal = goal }


setCoach : String -> State -> State
setCoach coach state =
    { state | coach = coach }


setPersonalBests : String -> State -> State
setPersonalBests pbs state =
    { state | personalBests = PersonalBests.readBests pbs }


setExercise : String -> State -> State
setExercise exercise state =
    let
        prev = ExercisesList.getPrev exercise

        next = ExercisesList.getNext exercise
    in
        { state
            | exercise = ( exercise, ExercisesList.getCategorie exercise )
            , prev = ( prev, ExercisesList.getCategorie prev )
            , next = ( next, ExercisesList.getCategorie next )
        }


stepSwitchExercise : Set.Set Char.KeyCode -> State -> State
stepSwitchExercise keys ({ prev, next, start, personalBests } as state) =
    let
        space = Set.member 32 keys

        p = Set.member 80 keys

        r = Set.member 82 keys

        n = Set.member 78 keys
    in
        if space && r then
            { state
                | editor =
                    (Editor.setDocument
                        start
                        Editor.initialState
                    )
                , keyHistory = KeyHistory.initialState
                , timeInMs = 0
                , waitForNoKeys = True
                , personalBests = personalBests
                , savedPB = False
                , finished = False
            }
        else if space && p then
            { state | redirectTo = exerciseLink (fst prev) }
        else if space && n then
            { state | redirectTo = exerciseLink (fst next) }
        else
            state


stepKeysEdit : Set.Set Char.KeyCode -> Int -> State -> State
stepKeysEdit inKeysDown time ({ editor, keyHistory, keysDown, goal } as state) =
    let
        keysDownNew = Set.diff inKeysDown keysDown |> Set.toList

        keysUpNew = Set.diff keysDown inKeysDown |> Set.toList
    in
        { state
            | editor = Editor.step editor keysDown keysDownNew
            , keyHistory =
                KeyHistory.step
                    keyHistory
                    keysDown
                    keysDownNew
                    keysUpNew
                    time
            , keysDown = inKeysDown
        }


generatePB : State -> PersonalBests.PB
generatePB { keyHistory, exercise } =
    let
        endTime = KeyHistory.getEndTime keyHistory

        dateStr = timeToString endTime

        name = fst exercise

        keys = KeyHistory.getKeyCount keyHistory

        time = KeyHistory.getTimeSpan keyHistory
    in
        { name = name
        , keys = keys
        , time = time
        , keysdate = dateStr
        , timedate = dateStr
        }


stepFinishedWithOldPB : PersonalBests.PB -> State -> State
stepFinishedWithOldPB oldPB ({ keyHistory, exercise, personalBests } as state) =
    let
        newPB = generatePB state

        newPBs = PersonalBests.insert personalBests newPB

        improvedBestKeys' = newPB.keys < oldPB.keys

        improvedBestTime' = newPB.time < oldPB.time

        alreadyHad5StarsEverywhere = fiveStarsInEverything personalBests

        nowHas5StarsEverywhere = fiveStarsInEverything newPBs
    in
        { state
            | personalBests = newPBs
            , improvedBestKeys = improvedBestKeys'
            , improvedBestTime = improvedBestTime'
            , thisTimePseudoPB = generatePB state
            , savedPB = improvedBestKeys' || improvedBestTime'
            , justGot5StarsEverywhere =
                not alreadyHad5StarsEverywhere && nowHas5StarsEverywhere
        }


stepFinishedWithOutOldPB : State -> State
stepFinishedWithOutOldPB ({ personalBests } as state) =
    let
        newPBs =
            PersonalBests.insert
                personalBests
                (generatePB state)

        alreadyHad5StarsEverywhere = fiveStarsInEverything personalBests

        nowHas5StarsEverywhere = fiveStarsInEverything newPBs
    in
        { state
            | personalBests = newPBs
            , savedPB = True
            , thisTimePseudoPB = generatePB state
            , justGot5StarsEverywhere =
                not alreadyHad5StarsEverywhere && nowHas5StarsEverywhere
        }


stepFinished : State -> State
stepFinished ({ personalBests, exercise } as state) =
    case PersonalBests.get personalBests (fst exercise) of
        Just pb ->
            stepFinishedWithOldPB pb state

        Nothing ->
            stepFinishedWithOutOldPB state


stepCheckFinished : State -> State
stepCheckFinished ({ finished, editor, goal } as state) =
    if finished then
        state
    else
        let
            finished' = editor.document == goal
        in
            if finished' then
                { state | finished = finished' } |> stepFinished
            else
                state


stepKeys : Set.Set Char.KeyCode -> Int -> State -> State
stepKeys inKeysDown time ({ editor, keyHistory, keysDown, goal, waitForNoKeys, personalBests, savedPB, exercise } as oldState) =
    let
        state = oldState |> stepCheckFinished

        state' =
            if state.finished then
                state
            else if state.waitForNoKeys && (not <| inKeysDown == Set.empty) then
                state
            else if state.waitForNoKeys && inKeysDown == Set.empty then
                { state
                    | waitForNoKeys = False
                    , keysDown = Set.empty
                }
            else if not <| waitForNoKeys then
                stepKeysEdit inKeysDown time state
            else
                state
    in
        state' |> stepSwitchExercise inKeysDown


main : Signal Element
main =
    Signal.map3 scene Window.width Window.height state


validKey : Char.KeyCode -> Bool
validKey key =
    KeyHistory.showKey key /= ""


displayGoal : String -> Element
displayGoal goal =
    Editor.displayDocument lightGray1 goal


showHeadline : String -> Element
showHeadline =
    toColoredSizedText green1 32


makeHeader : State -> Element
makeHeader { exercise, personalBests } =
    let
        header = String.concat [ snd exercise, " - ", fst exercise ]

        name = fst exercise

        pbsAndStarElem =
            case PersonalBests.get personalBests name of
                Just pb ->
                    flow
                        right
                        [ pb.keys |> toString |> toDefText
                        , spacer 10 1
                        , starsElemFromPBs False 3 personalBests name
                        , spacer 10 1
                        , pb.time |> showTimeInMs |> toDefText
                        ]

                Nothing ->
                    starsElemFromPBs False 3 personalBests name

        headLine = showHeadline header

        w = max (widthOf pbsAndStarElem) (widthOf headLine)
    in
        flow
            down
            [ headLine |> centerHorizontally w
            , pbsAndStarElem |> centerHorizontally w
            ]


scene : Int -> Int -> State -> Element
scene w h ({ editor, keyHistory, goal, timeInMs, coach, exercise } as state) =
    let
        headline = makeHeader state
    in
        flow
            down
            [ headline |> centerHorizontally w
            , scenePlay w h state |> centerHorizontally w
            ]
            |> Skeleton.showPage w h


showPressedKeys : Set.Set Char.KeyCode -> Element
showPressedKeys =
    Set.toList
        >> List.map KeyHistory.showKey
        >> String.join ","
        >> toDefText


showExercise : ( String, String ) -> (Text.Text -> Element) -> Element
showExercise ( name, cat ) align =
    String.concat [ "\n", name, "\n(", cat, ")" ]
        |> Text.fromString
        >> Text.height defTextSize
        >> Text.color green1
        >> align


exerciseLink : String -> String
exerciseLink str =
    if String.isEmpty str then
        ""
    else
        String.append "?page=game&exercise=" str


toExerciseLink : String -> Element -> Element
toExerciseLink =
    exerciseLink >> link


showPrev : ( String, String ) -> Element
showPrev ( name, cat ) =
    if String.isEmpty name then
        empty
    else
        flow
            right
            [ showExercise ( name, cat ) rightAligned
            , toColoredSizedText green1 82 " <-- "
            , toColText green1 "\n(space+p)"
            ]
            |> toExerciseLink name


showNext : ( String, String ) -> Element
showNext ( name, cat ) =
    if String.isEmpty name then
        empty
    else
        flow
            right
            [ toColText green1 "\n(space+n)"
            , toColoredSizedText green1 82 " --> "
            , showExercise ( name, cat ) leftAligned
            ]
            |> toExerciseLink name


showRestart : Element
showRestart =
    "\nrestart\n(space+r)"
        |> Text.fromString
        >> Text.height defTextSize
        >> Text.color green1
        >> centered


showButtons : Int -> State -> Element
showButtons w { exercise, prev, next } =
    flow
        outward
        [ showPrev prev
        , showRestart |> centerHorizontally w
        , showNext next |> showRightBottom w
        ]


coachResult : State -> List Element
coachResult { keyHistory, exercise, next, personalBests, improvedBestKeys, improvedBestTime, justGot5StarsEverywhere, thisTimePseudoPB } =
    let
        keyMoves = KeyHistory.getKeyCount keyHistory

        span = KeyHistory.getTimeSpan keyHistory

        name = fst exercise

        hasImprovedSomething = improvedBestKeys || improvedBestTime

        text =
            String.concat
                [ "You needed "
                , keyMoves |> toString
                , " key moves and "
                , span |> showTimeInMs |> String.dropRight 1
                , " seconds."
                , let
                    improveIntro = "\nThis means you have improved your personal best regarding "
                  in
                    if improvedBestKeys && improvedBestTime then
                        String.append improveIntro "key presses and time!"
                    else if improvedBestKeys then
                        String.append improveIntro "key presses!"
                    else if improvedBestTime then
                        String.append improveIntro "time!"
                    else
                        ""
                , if String.isEmpty (fst next) then
                    ""
                  else
                    String.concat
                        [ "\nYou can go on to the next exercise (space+n) or try to improve "
                        , if hasImprovedSomething then
                            "even more "
                          else
                            ""
                        , "(space+r)."
                        ]
                , if justGot5StarsEverywhere then
                    "\nBy the way, you now have 5 key stars and 5 time stars in all exercises.\nCongratulations, you are a true master now!"
                  else
                    ""
                ]
    in
        [ displayCoach text
        , starsElem False 1 name thisTimePseudoPB.keys thisTimePseudoPB.time
        ]


scenePlay : Int -> Int -> State -> Element
scenePlay winW winH ({ editor, keyHistory, goal, timeInMs, keysDown, coach, finished } as state) =
    let
        editorElem =
            flow
                down
                [ spacer 560 1 |> color gray1
                , spacer 1 3
                , "Your editor"
                    |> toColoredSizedText orange1 28
                    |> centerHorizontally 560
                , spacer 1 7
                , Editor.display editor
                ]

        goalElem =
            flow
                down
                [ spacer 560 1 |> color gray1
                , spacer 1 3
                , "Goal"
                    |> toColoredSizedText orange1 28
                    |> centerHorizontally 560
                , spacer 1 7
                , displayGoal goal
                ]

        middleElem =
            flow
                right
                [ editorElem
                , spacer 14 1
                , spacer 1 (max (heightOf editorElem) (heightOf goalElem))
                    |> color gray1
                , spacer 14 1
                , goalElem
                ]

        scoreElem =
            String.concat
                [ KeyHistory.getKeyCount keyHistory
                    |> toString
                , " / "
                , if finished then
                    KeyHistory.getTimeSpan keyHistory
                        |> showTimeInMs
                  else
                    timeInMs |> showTimeInDs
                ]
                |> toSizedText 32

        pressedKeysElem = showPressedKeys keysDown

        w = widthOf middleElem

        verticalScalerH = max 0 (230 - heightOf middleElem)

        coachElems =
            if finished then
                coachResult state
            else
                [ displayCoach coach ]
    in
        if String.isEmpty state.redirectTo then
            flow
                down
                [ flow
                    outward
                    [ KeyHistory.display 560 keyHistory
                    , scoreElem |> centerHorizontally w
                    , showRightBottom w pressedKeysElem
                    ]
                , spacer 1 3
                , middleElem
                , spacer 1 verticalScalerH
                , defaultSpacer
                , coachElems |> List.map (centerHorizontally w) |> flow down
                , showButtons w state
                ]
        else
            "loading ..." |> toDefText
