module Layout where

import Text

spacerSize : Int
spacerSize = 8

black1 : Color
black1 = rgb 39 40 34

white1 : Color
white1 = rgb 248 248 242

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




defaultSpacer : Element
defaultSpacer = spacer spacerSize spacerSize

doubleDefSpacer : Element
doubleDefSpacer = spacer ( 2 * spacerSize) (2 * spacerSize)

quadDefSpacer : Element
quadDefSpacer = spacer ( 4 * spacerSize) (4 * spacerSize)

pageWidth : Int
pageWidth = 450

toSizedText : Float -> String -> Element
toSizedText s = toText >> Text.height s >> Text.color white1 >> leftAligned

toSizedTextMod : (Text -> Text) -> Float -> String -> Element
toSizedTextMod f s =
  toText >> f >> Text.height s >> Text.color white1 >> leftAligned

toColText : Color -> String -> Element
toColText c =
  toText >> Text.height defTextSize >> Text.color c >> leftAligned

defTextSize : Float
defTextSize = 20

toDefText : String -> Element
toDefText = toSizedText defTextSize