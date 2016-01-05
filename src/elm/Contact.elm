module Contact (..) where

import Window
import Signal
import Markdown
import Graphics.Element exposing (Element, flow, down)
import Layout exposing (darkGray1, green1, white1, centerHorizontally, niceButton, defaultSpacer)
import Skeleton


exercisesButton : Element
exercisesButton =
    niceButton "Work out" "?page=exercises"


introduction : Int -> Element
introduction w =
    Skeleton.showTextPart w <| Markdown.toElement """

Whatever it is, just send me (Tobias Hermann) an email to [info@editgym.com](mailto:info@editgym.com). I am happy to hear from you.

"""


main : Signal Element
main =
    Signal.map2 scene Window.width Window.height


scene : Int -> Int -> Element
scene w h =
    flow
        down
        [ introduction w
        , defaultSpacer
        , centerHorizontally w exercisesButton
        ]
        |> Skeleton.showPage w h
