module PersonalBestList where

import Signal
import Window

import Markdown

import PersonalBests

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

port loadPBsIn : Signal String

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

todo: Show personal bests with values, dates and stars.

If you want to delete all your personal bests, click [here](?page=delete_personal_bests).

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  introduction w
  |> Skeleton.showPage w h