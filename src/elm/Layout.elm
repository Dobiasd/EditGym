module Layout where

import Text
import Color (Color, rgb)
import Graphics.Element (Element, spacer, container, widthOf, heightOf, middle)
import Graphics.Element

spacerSize : Int
spacerSize = 8

black1 : Color
black1 = rgb 39 40 34

white1 : Color
white1 = rgb 248 248 242

yellow1 : Color
yellow1 = rgb 230 219 116

orange1 : Color
orange1 = rgb 253 151 31

blue1 : Color
blue1 = rgb 102 217 239

purple1 : Color
purple1 = rgb 174 129 255

red1 : Color
red1 = rgb 249 38 114

green1 : Color
green1 = rgb 166 226 46

gray1 : Color
gray1 = rgb 117 113 94

lightGray1 : Color
lightGray1 = rgb 143 144 138

darkGray1 : Color
darkGray1 = rgb 56 56 48

-- todo rollover color, waiting for Evan:
-- https://groups.google.com/forum/#!topic/elm-discuss/IdbMAlFHAAE
niceButton : String -> String -> Element
niceButton = niceButtonSize 42

niceButtonSize : Float -> String -> String -> Element
niceButtonSize h str url =
  let makeText h c = Text.fromString
                     >> Text.height h
                     >> Text.color c
                     >> Text.leftAligned
      textElem = makeText h green1 str
      buttonW = 1.2 * (widthOf textElem |> toFloat) |> round
      buttonH = 1.3 * (heightOf textElem |> toFloat) |> round

      textButton = container buttonW buttonH
                             middle textElem
                   |> Graphics.Element.color darkGray1
  in  Graphics.Element.link url textButton

defaultSpacer : Element
defaultSpacer = spacer spacerSize spacerSize

doubleDefSpacer : Element
doubleDefSpacer = spacer (2 * spacerSize) (2 * spacerSize)

quadDefSpacer : Element
quadDefSpacer = spacer (4 * spacerSize) (4 * spacerSize)

divider : Color -> Int -> Element
divider col w = spacer w 1 |> Graphics.Element.color col

pageWidth : Int
pageWidth = 450

centerHorizontally : Int -> Element -> Element
centerHorizontally w element = container w (heightOf element) middle element

toSizedText : Float -> String -> Element
toSizedText s = Text.fromString >> Text.height s >> Text.color white1
                >> Text.leftAligned

toSizedTextMod : (Text.Text -> Text.Text) -> Float -> String -> Element
toSizedTextMod f s =
  Text.fromString >> f >> Text.height s >> Text.color white1 >> Text.leftAligned

toColText : Color -> String -> Element
toColText c =
  Text.fromString >> Text.height defTextSize >> Text.color c >> Text.leftAligned

defTextSize : Float
defTextSize = 20

toDefText : String -> Element
toDefText = toSizedText defTextSize