module Exercises where

import Graphics.Element (Element, flow, down, right, spacer, color)
import Window
import Signal
import List ((::))
import List

import Layout (toColText, toDefText, green1, blue1, darkGray1
  , niceButton, defaultSpacer, quadDefSpacer, toSizedText, niceButtonSize
  , centerHorizontally, divider, doubleDefSpacer)
import Skeleton
import Editor(safeHead)
import ExercisesList(..)

port loadPBsIn : Signal String

exerciseButton : String -> Element
exerciseButton s = niceButtonSize 24 s ("?page=game&exercise=" ++ s)

main : Signal Element
main = Signal.map2 scene Window.width Window.height

asGrid : Int -> List Element -> Element
asGrid colCnt =
  splitEvery colCnt
  >> List.map (List.intersperse quadDefSpacer >> flow right)
  >> List.intersperse quadDefSpacer
  >> flow down

showSubject : Int -> String -> List String -> Element
showSubject w subject exercises =
  let subjectElem = toSizedText 36 subject |> centerHorizontally w
      exercisesElem =
        exercises
        |> List.map exerciseButton
        |> asGrid 6
        |> centerHorizontally w
  in flow down [ subjectElem, doubleDefSpacer, exercisesElem ]

scene : Int -> Int -> Element
scene w h =
  let paddedDivider = flow down [
                          doubleDefSpacer
                        , divider blue1 w
                        , doubleDefSpacer
                      ]
  in  subjects
        |> List.map (showSubject w |> uncurry)
        |> List.intersperse paddedDivider
        |> flow down
        |> Skeleton.showPage w h