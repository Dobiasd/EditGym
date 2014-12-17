module Highscores where

import Signal
import Window

import Markdown

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

exercisesButton : Element
exercisesButton = niceButton "Work out" "?page=exercises"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

todo: list highscores of all exercises.

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w exercisesButton ]
  |> Skeleton.showPage w h