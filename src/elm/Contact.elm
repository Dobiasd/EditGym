module Contact where

import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

import Window

levelsButton : Element
levelsButton = niceButton "Work out" "?page=levels"

introduction : Int -> Element
introduction w =
  Skeleton.showTextPart w [markdown|

contact: info (at) editgym.com

|]

main : Signal Element
main = scene <~ Window.width ~ Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w levelsButton ]
  |> Skeleton.showPage w h