module CreateLevel where

import Signal
import Window

import Markdown

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

todo: Allow to create levels by linking files or using the url.

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  introduction w
  |> Skeleton.showPage w h