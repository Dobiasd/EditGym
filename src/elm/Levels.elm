module Levels where

import Layout (toColText, toDefText, green1, blue1, darkGray1, black1)

import Page
import Skeleton

import Text
import Graphics.Input as Input

levelClick : Input.Input String
levelClick = Input.input ""

levelButton : String -> Element
levelButton s =
  let txt h c = toText >> Text.height h >> Text.color c >> leftAligned
  in Input.customButton levelClick.handle s
    (color black1 (container 300 80 middle (txt 48 green1 s)))
    (color darkGray1 (container 300 80 middle (txt 48 green1 s)))
    (color darkGray1 (container 300 80 middle (txt 48 green1 s)))

display : Int -> Element
display w =
  flow down [
    levelButton "01 - foo"
  , levelButton "02 - bar"
  , levelButton "03 - baz"
  ] |> Skeleton.showPage w