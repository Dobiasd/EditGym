module Stars where

import Graphics.Element (Element, flow, outward, sizeOf, container, middle
    , right, spacer, down, heightOf)
import List(repeat, append, map2)
import String
import String(fromChar, slice, padRight)
import Text

import PersonalBests
import Layout(yellow1, gray1, darkGray1, toColoredSizedText)

referenceBestsJson : String
referenceBestsJson = """
[
    {
        \"name\": \"arrows\",
        \"keys\": 35,
        \"keysdate\": \"2014-12-21\",
        \"time\": 1038,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"batman\",
        \"keys\": 80,
        \"keysdate\": \"2014-12-21\",
        \"time\": 6400,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"growth\",
        \"keys\": 28,
        \"keysdate\": \"2014-12-21\",
        \"time\": 1291,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"indentation\",
        \"keys\": 61,
        \"keysdate\": \"2014-12-21\",
        \"time\": 2815,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"insert\",
        \"keys\": 42,
        \"keysdate\": \"2014-12-21\",
        \"time\": 1415,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"introduction\",
        \"keys\": 24,
        \"keysdate\": \"2014-12-21\",
        \"time\": 1151,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"new_lines\",
        \"keys\": 37,
        \"keysdate\": \"2014-12-21\",
        \"time\": 3264,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"numbers\",
        \"keys\": 172,
        \"keysdate\": \"2014-12-21\",
        \"time\": 10837,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"remove_duplicates\",
        \"keys\": 347,
        \"keysdate\": \"2014-12-21\",
        \"time\": 23024,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"remove_words_1\",
        \"keys\": 37,
        \"keysdate\": \"2014-12-21\",
        \"time\": 1752,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"remove_words_2\",
        \"keys\": 79,
        \"keysdate\": \"2014-12-21\",
        \"time\": 5599,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"replicate\",
        \"keys\": 14,
        \"keysdate\": \"2014-12-21\",
        \"time\": 552,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"surely\",
        \"keys\": 111,
        \"keysdate\": \"2014-12-21\",
        \"time\": 7887,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"swap_lines\",
        \"keys\": 15,
        \"keysdate\": \"2014-12-21\",
        \"time\": 634,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"swap_many_words\",
        \"keys\": 83,
        \"keysdate\": \"2014-12-21\",
        \"time\": 5792,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"swap_words\",
        \"keys\": 18,
        \"keysdate\": \"2014-12-21\",
        \"time\": 573,
        \"timedate\": \"2014-12-21\"
    },
    {
        \"name\": \"you_are_awesome\",
        \"keys\": 90,
        \"keysdate\": \"2014-12-21\",
        \"time\": 7220,
        \"timedate\": \"2014-12-21\"
    }
]"""

referenceBests : PersonalBests.PBs
referenceBests = PersonalBests.readBests referenceBestsJson

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