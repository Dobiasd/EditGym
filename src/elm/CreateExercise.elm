module CreateExercise (..) where

import Signal
import Window
import Markdown
import Graphics.Element exposing (Element, flow, down)
import Layout exposing (darkGray1, green1, white1, centerHorizontally, niceButton, defaultSpacer)
import Skeleton


introduction : Int -> Element
introduction w =
    Skeleton.showTextPart w <| Markdown.toElement """

This feature will enable you to provide custom texts for creating your own exercises.

It is not implemented yet.

"""


main : Signal Element
main =
    Signal.map2 scene Window.width Window.height


scene : Int -> Int -> Element
scene w h =
    introduction w
        |> Skeleton.showPage w h
