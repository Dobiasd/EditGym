module Stars where

import Graphics.Element (Element, flow, outward, sizeOf, container, middle
    , right, spacer, down, heightOf, empty)
import List(repeat, append, map2)
import String
import String(fromChar, slice, padRight)
import Text
import Maybe

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
        \"keys\": 16,
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
        \"keys\": 177,
        \"keysdate\": \"2014-12-21\",
        \"time\": 10418,
        \"timedate\": \"2014-12-21\"
    }
]
"""

referenceBests : PersonalBests.PBs
referenceBests = PersonalBests.readBests referenceBestsJson

toOnStarText : Float -> String -> Element
toOnStarText sizeDiv =
  if sizeDiv > 1.5
    then always empty
    else Text.fromString
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

scoreLine : Int -> Float -> Int -> Int
scoreLine ref fact val =
  let val' = toFloat val / toFloat ref
      normRes = -3/(fact - 1) * val' + 5 + 3/(fact - 1)
  in  normRes + 0.01 |> floor

scoreFunc : Int -> Float -> Int -> Int
scoreFunc ref fact val =
  let lineRes = scoreLine ref fact val
  in  max 1 (min 5 lineRes)

getTimeStarsElem : Float -> String -> Int -> Element
getTimeStarsElem sizeDiv name span =
  let starCnt = case PersonalBests.get referenceBests name of
                  Maybe.Just ref -> scoreFunc ref.time 5 span
                  Nothing -> 5
  in  getStarsWithString sizeDiv "Time" starCnt

getKeyMovesStarsElem : Float -> String -> Int -> Element
getKeyMovesStarsElem sizeDiv name keyMoves =
  let starCnt = case PersonalBests.get referenceBests name of
                  Maybe.Just ref -> scoreFunc ref.keys 2 keyMoves
                  Nothing -> 5
  in  getStarsWithString sizeDiv "Keys" starCnt

arrangeStarElems : Bool -> Float -> Element -> Element -> Element
arrangeStarElems below sizeDiv elem1 elem2 =
  let topDist = heightOf elem1 - (20 / sizeDiv |> round)
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

keyStarsElem : Bool -> Float -> String -> Int -> Int -> Element
keyStarsElem below sizeDiv name keyMoves timeSpan =
  let elem1 = getKeyMovesStarsElem sizeDiv name keyMoves
      elem2 = getTimeStarsElem sizeDiv name timeSpan
  in  arrangeStarElems below sizeDiv elem1 elem2

keyStarsElemFromPBs : Bool -> Float -> PersonalBests.PBs -> String -> Element
keyStarsElemFromPBs below sizeDiv pbs name =
  case PersonalBests.get pbs name of
    Just pb -> keyStarsElem False sizeDiv name pb.keys pb.time
    Nothing ->   let elem1 = getStarsWithString sizeDiv "Keys" 0
                     elem2 = getStarsWithString sizeDiv "Time" 0
                 in  arrangeStarElems below sizeDiv elem1 elem2