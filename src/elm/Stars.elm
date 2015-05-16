module Stars where

import Graphics.Element exposing (Element, flow, outward, sizeOf, container
  , middle, right, spacer, down, heightOf, empty, leftAligned)
import List exposing (repeat, append, map, map2, all, length)
import String
import String exposing (fromChar, slice, padRight)
import Text
import Maybe
import ExercisesList
import PersonalBests
import Layout exposing (yellow1, gray1, darkGray1, toColoredSizedText)

referenceBestsJson : String
referenceBestsJson = """
[
    {
        \"name\": \"arrows\",
        \"keys\": 18,
        \"keysdate\": \"2014-12-22\",
        \"time\": 1122,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"batman\",
        \"keys\": 41,
        \"keysdate\": \"2014-12-22\",
        \"time\": 6400,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"growth\",
        \"keys\": 15,
        \"keysdate\": \"2014-12-22\",
        \"time\": 1291,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"indentation\",
        \"keys\": 31,
        \"keysdate\": \"2014-12-22\",
        \"time\": 3194,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"insert\",
        \"keys\": 22,
        \"keysdate\": \"2014-12-22\",
        \"time\": 1537,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"introduction\",
        \"keys\": 13,
        \"keysdate\": \"2014-12-22\",
        \"time\": 1223,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"new_lines\",
        \"keys\": 19,
        \"keysdate\": \"2014-12-22\",
        \"time\": 3461,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"numbers\",
        \"keys\": 65,
        \"keysdate\": \"2014-12-22\",
        \"time\": 11223,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"remove_duplicates\",
        \"keys\": 79,
        \"keysdate\": \"2014-12-22\",
        \"time\": 25337,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"remove_words_1\",
        \"keys\": 13,
        \"keysdate\": \"2014-12-22\",
        \"time\": 1997,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"remove_words_2\",
        \"keys\": 27,
        \"keysdate\": \"2014-12-22\",
        \"time\": 5951,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"replicate\",
        \"keys\": 8,
        \"keysdate\": \"2014-12-22\",
        \"time\": 552,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"surely\",
        \"keys\": 55,
        \"keysdate\": \"2014-12-22\",
        \"time\": 7887,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"swap_lines\",
        \"keys\": 9,
        \"keysdate\": \"2014-12-22\",
        \"time\": 634,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"swap_many_lines\",
        \"keys\": 58,
        \"keysdate\": \"2014-12-22\",
        \"time\": 12535,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"swap_many_words\",
        \"keys\": 68,
        \"keysdate\": \"2014-12-22\",
        \"time\": 12257,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"swap_words\",
        \"keys\": 10,
        \"keysdate\": \"2014-12-22\",
        \"time\": 573,
        \"timedate\": \"2014-12-22\"
    },
    {
        \"name\": \"you_are_awesome\",
        \"keys\": 87,
        \"keysdate\": \"2014-12-22\",
        \"time\": 10418,
        \"timedate\": \"2014-12-22\"
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
         >> leftAligned

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

getKeyMovesStars : String -> Int -> Int
getKeyMovesStars name keyMoves =
  case PersonalBests.get referenceBests name of
    Maybe.Just ref -> scoreFunc ref.keys 2 keyMoves
    Nothing -> 5

getKeyMovesStarsElem : Float -> String -> Int -> Element
getKeyMovesStarsElem sizeDiv name keyMoves =
  getStarsWithString sizeDiv "Keys" (getKeyMovesStars name keyMoves)

getTimeStars: String -> Int -> Int
getTimeStars name span =
  case PersonalBests.get referenceBests name of
    Maybe.Just ref -> scoreFunc ref.time 5 span
    Nothing -> 5

getTimeStarsElem : Float -> String -> Int -> Element
getTimeStarsElem sizeDiv name span =
  getStarsWithString sizeDiv "Time" (getTimeStars name span)

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

starsElem : Bool -> Float -> String -> Int -> Int -> Element
starsElem below sizeDiv name keyMoves timeSpan =
  let elem1 = getKeyMovesStarsElem sizeDiv name keyMoves
      elem2 = getTimeStarsElem sizeDiv name timeSpan
  in  arrangeStarElems below sizeDiv elem1 elem2

starsFromPB : PersonalBests.PBs -> String -> (Int, Int)
starsFromPB pbs name =
  case PersonalBests.get pbs name of
    Just pb -> (getKeyMovesStars name pb.keys, getTimeStars name pb.time)
    Nothing -> (5, 5)

starsElemFromPBs : Bool -> Float -> PersonalBests.PBs -> String -> Element
starsElemFromPBs below sizeDiv pbs name =
  case PersonalBests.get pbs name of
    Just pb -> starsElem False sizeDiv name pb.keys pb.time
    Nothing ->   let elem1 = getStarsWithString sizeDiv "Keys" 0
                     elem2 = getStarsWithString sizeDiv "Time" 0
                 in  arrangeStarElems below sizeDiv elem1 elem2

fiveStarsInEverything : PersonalBests.PBs -> Bool
fiveStarsInEverything pbs =
  let names = ExercisesList.subjectsWithCat |> map fst
      starPairs = names |> map (starsFromPB pbs)
  in  all (\p -> p == (5, 5)) starPairs && length pbs >= length names