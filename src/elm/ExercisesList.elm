module ExercisesList (..) where

import List exposing ((::))
import List
import Editor exposing (safeHead)
import Unsafe exposing (unsafeHead, unsafeTail)


subjects : List ( String, List String )
subjects =
    [ ( "Warm up"
      , [ "introduction"
        , "arrows"
        , "replicate"
        , "swap_words"
        , "swap_lines"
        , "remove_words_1"
        ]
      )
    , ( "Strength"
      , [ "insert"
        , "new_lines"
        , "remove_words_2"
        , "surely"
        , "batman"
        ]
      )
    , ( "Endurance"
      , [ "numbers"
        , "swap_many_words"
        , "remove_duplicates"
        , "swap_many_lines"
        ]
      )
    , ( "Stretching"
      , [ "indentation"
        , "growth"
        , "you_are_awesome"
        ]
      )
    ]


subjectsWithCat : List ( String, String )
subjectsWithCat =
    let
        expand ( cat, exercises ) = List.map (\e -> ( e, cat )) exercises
    in
        subjects |> List.map expand |> List.concat


getPrev : String -> String
getPrev exercise =
    let
        pairs = List.map2 (,) (unsafeTail subjectsWithCat) subjectsWithCat

        founds = List.filter (\( ( e, _ ), _ ) -> e == exercise) pairs
    in
        if List.isEmpty founds then
            ""
        else
            founds |> unsafeHead |> snd |> fst


getNext : String -> String
getNext exercise =
    let
        pairs = List.map2 (,) (unsafeTail subjectsWithCat) subjectsWithCat

        founds = List.filter (\( _, ( e, _ ) ) -> e == exercise) pairs
    in
        if List.isEmpty founds then
            ""
        else
            founds |> unsafeHead |> fst |> fst


getCategorie : String -> String
getCategorie exercise =
    let
        founds = List.filter (\( e, _ ) -> e == exercise) subjectsWithCat
    in
        if List.isEmpty founds then
            ""
        else
            founds |> unsafeHead |> snd
