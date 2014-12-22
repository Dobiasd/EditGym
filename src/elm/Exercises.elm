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
  , centerHorizontally, divider, doubleDefSpacer, displayCoach, asGrid)
import Skeleton
import Editor(safeHead)
import ExercisesList(..)
import Stars(starsElemFromPBs, fiveStarsInEverything, getStarsWithString
  , arrangeStarElems)

port loadPBsIn : Signal String

personalBests : Signal PersonalBests.PBs
personalBests = Signal.map PersonalBests.readBests
  (Signal.dropRepeats loadPBsIn)

exerciseButton : String -> Element
exerciseButton s = niceButtonSize 24 s ("?page=game&exercise=" ++ s)

main : Signal Element
main = Signal.map3 scene Window.width Window.height personalBests

exerciseButtonWithStars : PersonalBests.PBs -> String -> Element
exerciseButtonWithStars pbs name =
  let starsElem = starsElemFromPBs False 4 pbs name
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
        |> asGrid 6 quadDefSpacer
        |> centerHorizontally w
  in flow down [ subjectElem, doubleDefSpacer, exercisesElem ]

fiveStarsInEverythingElem : Int -> Element
fiveStarsInEverythingElem w =
  flow down [
      arrangeStarElems False 1.5 (getStarsWithString 0.5 "Keys" 5)
                                 (getStarsWithString 0.5 "Time" 5)
      |> centerHorizontally w
    , displayCoach "You rock!"
      |> centerHorizontally w
  ]

scene : Int -> Int -> PersonalBests.PBs -> Element
scene w h pbs =
  let paddedDivider = flow down [
                          doubleDefSpacer
                        , divider blue1 w
                        , doubleDefSpacer
                      ]
      allDone = fiveStarsInEverything pbs
  in  subjects
        |> List.map (showSubject w pbs |> uncurry)
        |> \x -> x ++ (if allDone then [fiveStarsInEverythingElem w] else [])
        |> List.intersperse paddedDivider
        |> flow down
        |> Skeleton.showPage w h