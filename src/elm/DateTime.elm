module DateTime (..) where

import Date
import String
import Time


type alias DateAsInts =
    { year : Int, month : Int, day : Int }


timeToDateAsInts : Time.Time -> DateAsInts
timeToDateAsInts =
    Date.fromTime >> dateToDateAsInts


dateToDateAsInts : Date.Date -> DateAsInts
dateToDateAsInts date =
    DateAsInts (Date.year date) (Date.month date |> monthToInt) (Date.day date)


monthToInt m =
    case m of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


showDateAsInts : DateAsInts -> String
showDateAsInts intDate =
    let
        pad = String.padLeft 2 '0'

        yearStr = toString intDate.year

        monthStr = toString intDate.month |> pad

        dayStr = toString intDate.day |> pad
    in
        String.join "-" [ yearStr, monthStr, dayStr ]


timeToString : Int -> String
timeToString =
    toFloat >> timeToDateAsInts >> showDateAsInts
