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
    levelButton "01 - foo"
  , levelButton "02 - bar"
  , levelButton "03 - baz"
  ] |> intersperse defaultSpacer |> flow down |> Skeleton.showPage w h