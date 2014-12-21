module Exercises where

import Graphics.Element (Element, flow, down, right, spacer, color, empty
  , widthOf)
import Window
import Signal
import List ((::))
import List
import PersonalBests

import Layout (toColText, toDefText, green1, blue1, darkGray1
  , niceButton, defaultSpacer, quadDefSpacer, toSizedText, niceButtonSize
  , centerHorizontally, divider, doubleDefSpacer)
import Skeleton
import Editor(safeHead)
import ExercisesList(..)
import Stars(keyStarsElemFromPBs)

port loadPBsIn : Signal String

personalBests : Signal PersonalBests.PBs
personalBests = Signal.map PersonalBests.readBests
  (Signal.dropRepeats loadPBsIn)

exerciseButton : String -> Element
exerciseButton s = niceButtonSize 24 s ("?page=game&exercise=" ++ s)

main : Signal Element
main = Signal.map3 scene Window.width Window.height personalBests

asGrid : Int -> List Element -> Element
asGrid colCnt =
  splitEvery colCnt
  >> List.map (List.intersperse quadDefSpacer >> flow right)
  >> List.intersperse quadDefSpacer
  >> flow down

exerciseButtonWithStars : PersonalBests.PBs -> String -> Element
exerciseButtonWithStars pbs name =
  let starsElem = keyStarsElemFromPBs False 4 pbs name
      btn = exerciseButton name
      w = max (widthOf starsElem) (widthOf btn)
  in  flow down [
          starsElem |> centerHorizontally w
        , btn |> centerHorizontally w
      ]

showSubject : Int -> PersonalBests.PBs -> String -> List String -> Element
showSubject w pbs subject exercises =
  let subjectElem = toSizedText 36 subject |> centerHorizontally w
      exercisesElem =
        exercises
        |> List.map (exerciseButtonWithStars pbs)
        |> asGrid 6
        |> centerHorizontally w
  in flow down [ subjectElem, doubleDefSpacer, exercisesElem ]

scene : Int -> Int -> PersonalBests.PBs -> Element
scene w h pbs =
  let paddedDivider = flow down [
                          doubleDefSpacer
                        , divider blue1 w
                        , doubleDefSpacer
                      ]
  in  subjects
        |> List.map (showSubject w pbs |> uncurry)
        |> List.intersperse paddedDivider
        |> flow down
        |> Skeleton.showPage w h