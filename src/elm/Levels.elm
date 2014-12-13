module Levels where

import Layout (toColText, toDefText, green1, blue1, darkGray1, black1
  , niceButton, defaultSpacer)

import Skeleton

import Window

levelButton : String -> Element
levelButton s = niceButton s ("?page=game&level=" ++ s)

main : Signal Element
main = scene <~ Window.width ~ Window.height

scene : Int -> Int -> Element
scene w h =
  [
    levelButton "line_swap"
  , levelButton "word_swap"
  , levelButton "remove_words"
  , levelButton "insert"
  ] |> intersperse defaultSpacer |> flow down |> Skeleton.showPage w h