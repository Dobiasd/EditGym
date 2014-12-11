module Header where

import Graphics.Input (Input, input)
import Window

import Layout (defaultSpacer, toDefText, toSizedText,
  black1, white1, orange1, blue1, purple1, red1, green1, gray1)
import Text

iconSize : Int
iconSize = 32

logoHeight : Int
logoHeight = 120

logoWidth : Int
logoWidth = 308

clicks : Input ()
clicks = input ()

shareIcons : Element
shareIcons =
  let
    buttons =
      [ ( image iconSize iconSize "imgs/facebook.png", "https://www.facebook.com/sharer/sharer.php?u=http://www.editgym.com" )
      , ( image iconSize iconSize "imgs/twitter.png", "https://twitter.com/home?status=Text%20editing%20training%20at%20http://www.editgym.com" )
      , ( image iconSize iconSize "imgs/googleplus.png", "https://plus.google.com/share?url=http://www.editgym.com" )
      , ( image iconSize iconSize "imgs/linkedin.png", "https://www.linkedin.com/shareArticle?mini=true&url=http://www.editgym.com&title=Text%20editing%20training&source=" )
      , ( image iconSize iconSize "imgs/pinterest.png", "https://pinterest.com/pin/create/button/?url=&media=http://www.editgym.com&description=Text%20editing%20training" )
      , ( image iconSize iconSize "imgs/digg.png", "http://digg.com/submit?phase=2&url=http://www.editgym.com&title=Text%20editing%20training" )
      , ( image iconSize iconSize "imgs/stumbleupon.png", "http://www.stumbleupon.com/submit?url=http://www.editgym.com&title=Text%20editing%20training" )
      , ( image iconSize iconSize "imgs/tumblr.png", "http://www.tumblr.com/share/link?url=editgym.com" )
      , ( image iconSize iconSize "imgs/bufferapp.png", "https://bufferapp.com/add?url=http://www.editgym.com&text=Text%20editing%20training" )
      , ( image iconSize iconSize "imgs/email.png", "mailto:%20?subject=reddit time machine&body=Text%20editing%20training%20at%20http://www.editgym.com" ) ]
      |> map (\ (img, url) -> img |> link url)
  in
    toDefText "share: " :: buttons |> intersperse (defaultSpacer) |> flow right

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