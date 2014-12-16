module Levels where

import Graphics.Element (Element, flow, down, right, spacer, color)
import Window
import Signal
import List ((::))
import List

import Layout (toColText, toDefText, green1, blue1, darkGray1, black1
  , niceButton, defaultSpacer, quadDefSpacer, toSizedText, niceButtonSize
  , centerHorizontally, divider, doubleDefSpacer)
import Skeleton

levelButton : String -> Element
levelButton s = niceButtonSize 28 s ("?page=game&level=" ++ s)

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
        "just_type"
      , "delete_chars"
      , "line_swap"
      , "word_swap"
      , "remove_words_1"
      ]
    )
  , ( "Strength"
    , [
        "insert"
      , "new_lines"
      , "remove_words_2"
      , "replace_words"
      , "batman"
      ]
    )
  , ( "Endurance"
    , [
        "numbers"
      , "swap_many_words"
      ]
    )
  , ( "Stretching"
    , [
        "i_am_awesome"
      , "more_lines"
      ]
    )
  ]

showSubject : Int -> String -> List String -> Element
showSubject w subject levels =
  let subjectElem = toSizedText 48 subject |> centerHorizontally w
      levelsElem =
        levels
        |> List.map levelButton
        |> asGrid 3
        |> centerHorizontally w
  in flow down [ subjectElem, doubleDefSpacer, levelsElem ]

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