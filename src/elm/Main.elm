module Main where

import Window

import Skeleton (showPage)

main : Signal Element
main = scene <~ Window.width

scene : Int -> Element
scene w =
  let content = plainText "EditGym"
  in  showPage w content