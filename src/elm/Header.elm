module Header where

import Window
import List
import List ((::))
import Graphics.Element (Element, container, topRight, color, image, link, flow, down, right)
import Text

import UrlEncode (genLink)
import Layout (defaultSpacer, toDefText, toSizedText,
  black1, white1, orange1, blue1, purple1, red1, green1, gray1)

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
  in
    toDefText "share: " :: buttons
      |> List.intersperse (defaultSpacer)
      |> flow right

logo : Element
logo = image logoWidth logoHeight "imgs/logo.png" |> link "?page=start"

topBar : Int -> Element
topBar w =
  flow down [ defaultSpacer
            , flow right [ logo
            , flow right [ shareIcons, defaultSpacer ]
                |> container (w - logoWidth) logoHeight topRight
            ]
            , defaultSpacer ] |> color black1

header : Int -> Element
header w =
  flow down [
    topBar w
  , defaultSpacer ] |> color black1