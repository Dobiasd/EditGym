module Game where

import Set

import Layout (toDefText)
import Skeleton
import Page
import Editor
import KeyHistory

type State = {
    editor : Editor.State
  , keyHistory : KeyHistory.State
  , keysDown : Set.Set Int
  }

initialState : State
initialState = State Editor.initialState KeyHistory.initialState Set.empty

type Input = { inKeysDown : Set.Set Int }

step : Input -> State -> State
step {inKeysDown} ({editor, keyHistory, keysDown} as state) =
  let keysDownNew = Set.diff inKeysDown keysDown
  in  { state | editor <- Editor.step editor keysDown keysDownNew
              , keyHistory <- KeyHistory.step keyHistory keysDown keysDownNew
              , keysDown <- inKeysDown }

display : Int -> State -> Element
display w {editor, keyHistory} =
  let content = flow down [
                    KeyHistory.display keyHistory
                  , Editor.display editor
                ]
  in  Skeleton.showPage w content