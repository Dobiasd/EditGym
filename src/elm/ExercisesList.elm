module ExercisesList where

import List ((::))
import List
import Editor(safeHead)

{-| splitEvery [1,2,3,4,5,6,7,8] === [[1,2,3],[4,5,6],[7,8]] -}
splitEvery : Int -> List a -> List (List a)
splitEvery n xs =
  if List.length xs > n
    then (List.take n xs) :: (List.drop n xs |> splitEvery n)
    else [xs]

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