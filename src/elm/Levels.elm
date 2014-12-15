module Levels where

import Graphics.Element (Element, flow, down)
import Window
import Signal
import List(intersperse)

import Layout (toColText, toDefText, green1, blue1, darkGray1, black1
  , niceButton, defaultSpacer)
import Skeleton

levelButton : String -> Element
levelButton s = niceButton s ("?page=game&level=" ++ s)

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  [
    levelButton "word_swap"
  , levelButton "line_swap"
  , levelButton "insert"
  , levelButton "remove_words"
  ] |> intersperse defaultSpacer |> flow down |> Skeleton.showPage w h