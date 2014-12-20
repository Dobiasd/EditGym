module Stars where

import Graphics.Element (Element, flow, outward, sizeOf, container, middle
    , right, spacer, down, heightOf)
import List(repeat, append, map2)
import String
import String(fromChar, slice, padRight)
import Text

import Layout(yellow1, gray1, darkGray1, toColoredSizedText)

toOnStarText : Float -> String -> Element
toOnStarText sizeDiv =
  Text.fromString
  >> Text.bold
  >> Text.height (12 / sizeDiv)
  >> Text.color gray1
  >> Text.leftAligned

starWithChar : Float -> Bool -> Char -> Element
starWithChar sizeDiv filled c =
  let (col, starStr) = if filled then (yellow1, "★") else (darkGray1, "☆")
      starElem = toColoredSizedText col (48 / sizeDiv) starStr
      charElem = toOnStarText sizeDiv (fromChar c)
      (w, h) = sizeOf starElem
  in  flow outward [
          starElem
        , container w h middle charElem
      ]

getStarsWithString : Float -> String -> Int -> Element
getStarsWithString sizeDiv str fullCnt =
  let str' = padRight 5 ' ' str |> slice 0 5
      fullCnt' = min 5 (max 0 fullCnt)
      starFills = append (repeat fullCnt' True) (repeat (5 - fullCnt') False)
  in flow right <| map2 (starWithChar sizeDiv) starFills (String.toList str')

getTimeStarsElem : Float -> String -> Int -> Element
getTimeStarsElem sizeDiv name span =
  getStarsWithString sizeDiv "Time" 2

getKeyMovesStarsElem : Float -> String -> Int -> Element
getKeyMovesStarsElem sizeDiv name keyMoves =
  getStarsWithString sizeDiv "Keys" 5

keyStarsElem : Bool -> Float -> String -> Int -> Int -> Element
keyStarsElem below sizeDiv name keyMoves timeSpan =
  let elem1 = getKeyMovesStarsElem sizeDiv name keyMoves
      elem2 = getTimeStarsElem sizeDiv name timeSpan
      topDist = heightOf elem1 - (20 / sizeDiv |> round)
  in  if below
         then flow outward [
                  elem1
                , flow down [ spacer 1 topDist, elem2 ]
              ]
         else flow right [
                  elem1
                , spacer (32 / sizeDiv |> round) 1
                , elem2
              ]