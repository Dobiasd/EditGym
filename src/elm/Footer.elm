module Footer where

import List
import Graphics.Element (Element, container, heightOf, widthOf, middle
    , color, link, flow, down, right, midTop)
import Color (Color)

import Layout (defaultSpacer, darkGray1, toDefText, divider, purple1
    , centerHorizontally, quadDefSpacer)

import Header (menu)

txtLink str url =
  toDefText str |> link url

menuItems : List (String, String)
menuItems = [
    ("FAQ", "?page=faq")
  , ("Newsletter", "?page=newsletter")
  , ("Contact", "?page=contact")
  ]

footer : Int -> Element
footer w =
  let
    buttons = menuItems
              |> menu quadDefSpacer
              |> centerHorizontally w
    content = flow down [ defaultSpacer
                         , divider purple1 w
                         , defaultSpacer
                         , buttons
                         , defaultSpacer ]
  in
    content |> container w (heightOf content) midTop