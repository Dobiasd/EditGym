module PersonalBestList where

import Signal
import Window
import String
import List

import Markdown

import PersonalBests

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer, showTimeInMs, toDefText)
import Skeleton

port loadPBsIn : Signal String

personalBests : Signal PersonalBests.PBs
personalBests = Signal.map PersonalBests.readBests
  (Signal.dropRepeats loadPBsIn)

deleteText : String
deleteText = """

---

If you want to delete all your personal bests, click [here](?page=delete_personal_bests).
"""

showBest : PersonalBests.PB -> String
showBest {name, keys, keysdate, time, timedate} =
  String.concat [
      "##", name, "\n"
    , "\n    Key movements: ", toString keys, " (", keysdate, ")"
    , "\n    Time: ", showTimeInMs time, " (", timedate, ")"
  ]

genBestList : PersonalBests.PBs -> String
genBestList personalBests =
  String.concat [
      "#Your personal bests\n\n"
      , List.map showBest personalBests |> String.join "\n\n"
  ]

genText : PersonalBests.PBs -> String
genText personalBests = String.append (genBestList personalBests) deleteText

main : Signal Element
main = Signal.map3 scene Window.width Window.height personalBests

scene : Int -> Int -> PersonalBests.PBs -> Element
scene w h personalBests =
  genText personalBests
  |> Markdown.toElement
  |> Skeleton.showTextPart w
  |> Skeleton.showPage w h