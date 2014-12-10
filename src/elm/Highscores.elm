module Highscores where

import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

import Window

levelsButton : Element
levelsButton = niceButton "Work out" "?page=levels"

introduction : Int -> Element
introduction w =
  let content = [markdown|

todo: list highscores of all levels.

|] |> width 640
      img = image 800 280 "imgs/keyboard_bg.jpg"
      row = collage 800 280 [toForm img, toForm content]
  in  container w 280 middle row |> color white1

main : Signal Element
main = scene <~ Window.width ~ Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w levelsButton ]
  |> Skeleton.showPage w h