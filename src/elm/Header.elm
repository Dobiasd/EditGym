module Header where

import Window
import List
import List ((::))
import Graphics.Element (Element, container, topRight, color, image, link, flow
  , down, right, widthOf, heightOf, middle, bottomRight, outward, spacer)
import Text

import UrlEncode (genLink)
import Layout (defaultSpacer, toDefText, toSizedText, octaDefSpacer
  , white1, orange1, blue1, purple1, red1, green1, gray1
  , divider, darkGray1, lightGray1, doubleDefSpacer, centerHorizontally
  , tripleDefSpacer, toColoredSizedText, quadDefSpacer
  , showRightTop)

iconSize : Int
iconSize = 32

logoHeight : Int
logoHeight = 120

logoWidth : Int
logoWidth = 308

shareIcons : Element
shareIcons =
  let
    buttons =
      [ ( image iconSize iconSize "imgs/facebook.png", genLink "https://www.facebook.com/sharer/sharer.php" [("u", "http://www.editgym.com")] )
      , ( image iconSize iconSize "imgs/twitter.png", genLink "https://twitter.com/home" [("status", "Text editing training at http://www.editgym.com")] )
      , ( image iconSize iconSize "imgs/googleplus.png", genLink "https://plus.google.com/share" [("url", "http://www.editgym.com")] )
      , ( image iconSize iconSize "imgs/linkedin.png", genLink "https://www.linkedin.com/shareArticle" [("mini", "true"), ("url", "http://www.editgym.com"), ("title", "Text editing training"), ("source", "" )] )
      , ( image iconSize iconSize "imgs/pinterest.png", genLink "https://pinterest.com/pin/create/button/" [("url", ""), ("media", "http://www.editgym.com"), ("description", "Text editing training")] )
      , ( image iconSize iconSize "imgs/digg.png", genLink "http://digg.com/submit" [("phase", "2"), ("url", "http://www.editgym.com"), ("title", "Text editing training")] )
      , ( image iconSize iconSize "imgs/stumbleupon.png", genLink "http://www.stumbleupon.com/submit" [("url", "http://www.editgym.com"), ("title", "Text editing training")] )
      , ( image iconSize iconSize "imgs/tumblr.png", genLink "http://www.tumblr.com/share/link" [("url", "editgym.com")] )
      , ( image iconSize iconSize "imgs/bufferapp.png", genLink "https://bufferapp.com/add" [("url", "http://www.editgym.com"), ("text", "Text editing training")] )
      , ( image iconSize iconSize "imgs/email.png", genLink "mailto: " [("subject", "EditGym"), ("body", "Text editing training at http://www.editgym.com")] ) ]
      |> List.map (\ (img, url) -> img |> link url)
  in  buttons
      |> List.intersperse (defaultSpacer)
      |> \xs -> xs ++ [defaultSpacer]
      |> flow right

logo : Element
logo = image logoWidth logoHeight "imgs/logo.png" |> link "?page=start"

menuButton str url =
    let toTxt = Text.fromString
                >> Text.height 18
                >> Text.color lightGray1
                >> Text.leftAligned
    in  toTxt str |> link url

menuItems : List (String, String)
menuItems = [
    ("Home", "?page=start")
  , ("Exercises", "?page=exercises")
  , ("High scores", "?page=highscores")
  , ("Create exercise", "?page=create_exercise")
  , ("Help", "?page=help")
  ]

menu : Element -> List (String, String) -> Element
menu distSpacer =
  List.map (uncurry menuButton)
  >> List.intersperse distSpacer
  >> flow right

topBar : Int -> Element -> Element
topBar w headline =
  let buttons = menu tripleDefSpacer menuItems
      rightPartW = w - logoWidth
      menuSpacerW = (rightPartW - widthOf buttons) // 2
      shareLeftSpacerElemW = w - (widthOf shareIcons + logoWidth
                                + widthOf headline )
      headlineSpacerW = shareLeftSpacerElemW // 2
  in  flow down [
          spacer 1 3
        , flow outward [
              logo
            , flow down [
                headline |> centerHorizontally w
              , spacer 1
                  (heightOf logo - (heightOf headline + heightOf buttons))
              , buttons |> centerHorizontally w
            ]
            , flow down [ spacer 1 5, shareIcons |> showRightTop w ]
          ]
      ]

header : Int -> Element -> Element
header w headline =
  flow down [
    topBar w headline
  , spacer 1 3
  , divider purple1 w
  , spacer 1 3
  ]