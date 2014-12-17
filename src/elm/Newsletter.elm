module Newsletter where

import Window
import Signal

import Markdown

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

levelsButton : Element
levelsButton = niceButton "Work out" "?page=levels"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

Coach: Do you me to inform you about updates like new levels and stuff?

Just tell me your email address and I'll inform you.

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w levelsButton ]
  |> Skeleton.showPage w h