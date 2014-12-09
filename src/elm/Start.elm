module Start where

import Layout (toColText, toDefText, green1, blue1, darkGray1, black1)

import Page
import Skeleton

import Text
import Graphics.Input as Input

startClick : Input.Input Page.Page
startClick = Input.input Page.NoPage

startButton : Element
startButton =
  let txt h c = toText >> Text.height h >> Text.color c >> leftAligned
  in Input.customButton startClick.handle Page.Levels
    (color black1 (container 300 80 middle (txt 48 green1 "Start")))
    (color darkGray1 (container 300 80 middle (txt 48 green1 "Start!")))
    (color darkGray1 (container 300 80 middle (txt 48 green1 "Start :)")))

display : Int -> Element
display w = startButton |> Skeleton.showPage w