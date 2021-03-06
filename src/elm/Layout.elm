module Layout (..) where

import Text
import List exposing ((::))
import Color exposing (Color, rgb)
import Graphics.Element exposing (Element, spacer, container, widthOf, heightOf, middle, flow, outward, bottomRight, topRight, right, down, leftAligned)
import String


{-| splitEvery [1,2,3,4,5,6,7,8] === [[1,2,3],[4,5,6],[7,8]]
-}
splitEvery : Int -> List a -> List (List a)
splitEvery n xs =
    if List.length xs > n then
        (List.take n xs) :: (List.drop n xs |> splitEvery n)
    else
        [ xs ]


asGrid : Int -> Element -> List Element -> Element
asGrid colCnt spacer =
    splitEvery colCnt
        >> List.map (List.intersperse spacer >> flow right)
        >> List.intersperse spacer
        >> flow down


spacerSize : Int
spacerSize =
    8


black1 : Color
black1 =
    rgb 39 40 34


white1 : Color
white1 =
    rgb 248 248 242


yellow1 : Color
yellow1 =
    rgb 230 219 116


orange1 : Color
orange1 =
    rgb 253 151 31


blue1 : Color
blue1 =
    rgb 102 217 239


purple1 : Color
purple1 =
    rgb 174 129 255


red1 : Color
red1 =
    rgb 249 38 114


green1 : Color
green1 =
    rgb 166 226 46


gray1 : Color
gray1 =
    rgb 117 113 94


lightGray1 : Color
lightGray1 =
    rgb 143 144 138


darkGray1 : Color
darkGray1 =
    rgb 56 56 48



-- todo rollover color, waiting for Evan:
-- https://groups.google.com/forum/#!topic/elm-discuss/IdbMAlFHAAE


niceButton : String -> String -> Element
niceButton =
    niceButtonSize 42


displayCoach : String -> Element
displayCoach str =
    let
        textElem =
            str
                |> Text.fromString
                |> Text.height 20
                |> Text.color blue1
                |> leftAligned
    in
        flow
            right
            [ toColText blue1 "Coach: "
            , textElem
            ]


niceButtonSize : Float -> String -> String -> Element
niceButtonSize h str url =
    let
        makeText h c =
            Text.fromString
                >> Text.height h
                >> Text.color c
                >> leftAligned

        textElem = makeText h green1 str

        border = 0.14 * (heightOf textElem |> toFloat) |> round

        buttonW = widthOf textElem + 6 * border

        buttonH = heightOf textElem + border

        textButton =
            container
                buttonW
                buttonH
                middle
                textElem
                |> Graphics.Element.color darkGray1
    in
        Graphics.Element.link url textButton


defaultSpacer : Element
defaultSpacer =
    spacer spacerSize spacerSize


doubleDefSpacer : Element
doubleDefSpacer =
    spacer (2 * spacerSize) (2 * spacerSize)


tripleDefSpacer : Element
tripleDefSpacer =
    spacer (3 * spacerSize) (3 * spacerSize)


quadDefSpacer : Element
quadDefSpacer =
    spacer (4 * spacerSize) (4 * spacerSize)


octaDefSpacer : Element
octaDefSpacer =
    spacer (8 * spacerSize) (8 * spacerSize)


divider : Color -> Int -> Element
divider col w =
    spacer w 1 |> Graphics.Element.color col


centerHorizontally : Int -> Element -> Element
centerHorizontally w element =
    container w (heightOf element) middle element


showRightBottom : Int -> Element -> Element
showRightBottom w element =
    container w (heightOf element) bottomRight element


showRightTop : Int -> Element -> Element
showRightTop w element =
    container w (heightOf element) topRight element


toSizedText : Float -> String -> Element
toSizedText =
    toColoredSizedText white1


toColoredSizedText : Color -> Float -> String -> Element
toColoredSizedText col s =
    Text.fromString
        >> Text.height s
        >> Text.color col
        >> leftAligned


toSizedTextMod : (Text.Text -> Text.Text) -> Float -> String -> Element
toSizedTextMod f s =
    Text.fromString >> f >> Text.height s >> Text.color white1 >> leftAligned


toColText : Color -> String -> Element
toColText c =
    Text.fromString >> Text.height defTextSize >> Text.color c >> leftAligned


defTextSize : Float
defTextSize =
    20


toDefText : String -> Element
toDefText =
    toSizedText defTextSize


showTimeInPrec : Int -> Int -> String
showTimeInPrec decimals timeInMs =
    let
        str = timeInMs |> toString |> String.dropRight (3 - decimals)

        part2 = String.right decimals str
    in
        String.concat
            [ if timeInMs < 1000 then
                "0"
              else
                ""
            , String.dropRight decimals str
            , "."
            , if String.isEmpty part2 then
                "0"
              else
                part2
            , "s"
            ]


showTimeInDs : Int -> String
showTimeInDs =
    showTimeInPrec 1


showTimeInMs : Int -> String
showTimeInMs =
    showTimeInPrec 3
