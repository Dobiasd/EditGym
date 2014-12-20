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

exerciseButton : String -> Element
exerciseButton s = niceButtonSize 26 s ("?page=game&exercise=" ++ s)

main : Signal Element
main = Signal.map2 scene Window.width Window.height

{-| splitEvery [1,2,3,4,5,6,7,8] === [[1,2,3],[4,5,6],[7,8]] -}
splitEvery : Int -> List a -> List (List a)
splitEvery n xs =
  if List.length xs > n
    then (List.take n xs) :: (List.drop n xs |> splitEvery n)
    else [xs]

asGrid : Int -> List Element -> Element
asGrid colCnt =
  splitEvery colCnt
  >> List.map (List.intersperse quadDefSpacer >> flow right)
  >> List.intersperse quadDefSpacer
  >> flow down

subjects : List (String, List String)
subjects =
  [ ( "Warm up"
    , [
        "introduction"
      , "arrows"
      , "replicate"
      , "swap_words"
      , "swap_lines"
      , "remove_words_1"
      ]
    )
  , ( "Strength"
    , [
        "insert"
      , "new_lines"
      , "remove_words_2"
      , "surely"
      , "batman"
      ]
    )
  , ( "Endurance"
    , [
        "numbers"
      , "swap_many_words"
      , "remove_duplicates"
      ]
    )
  , ( "Stretching"
    , [
        "indentation"
      , "growth"
      , "you_are_awesome"
      ]
    )
  ]

subjectsWithCat : List (String, String)
subjectsWithCat =
  let expand (cat, exercises) = List.map (\e -> (e, cat)) exercises
  in  subjects |> List.map expand |> List.concat

getPrev : String -> String
getPrev exercise =
  let pairs = List.map2 (,) (List.tail subjectsWithCat) subjectsWithCat
      founds = List.filter (\((e, _), _) -> e == exercise) pairs
  in  if List.isEmpty founds then "" else founds |> List.head |> snd |> fst

getNext : String -> String
getNext exercise =
  let pairs = List.map2 (,) (List.tail subjectsWithCat) subjectsWithCat
      founds = List.filter (\(_, (e, _)) -> e == exercise) pairs
  in  if List.isEmpty founds then "" else founds |> List.head |> fst |> fst

getCategorie : String -> String
getCategorie exercise =
  let founds = List.filter (\(e, _) -> e == exercise) subjectsWithCat
  in  if List.isEmpty founds then "" else founds |> List.head |> snd

showSubject : Int -> String -> List String -> Element
showSubject w subject exercises =
  let subjectElem = toSizedText 40 subject |> centerHorizontally w
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