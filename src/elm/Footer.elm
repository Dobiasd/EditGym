module Footer where

import List
import Graphics.Element (Element, container, heightOf, widthOf, middle
    , color, link, flow, down, right, midTop)
import Color (Color)

import Layout (defaultSpacer, darkGray1, toDefText, divider, purple1
    , centerHorizontally, doubleDefSpacer)

txtLink str url =
    let elem = toDefText str
    in  container (widthOf elem + 10) (heightOf elem + 6) middle elem
        |> color darkGray1 |> link url

menuItems : List (String, String)
menuItems = [
    ("Home", "?page=start")
  , ("Highscore list", "?page=highscores")
  , ("FAQ", "?page=faq")
  , ("Levels", "?page=levels")
  , ("Create level", "?page=create_level")
  , ("Help", "?page=help")
  , ("Contact", "?page=contact")
  ]

footer : Int -> Element
footer w =
  let
    buttons = menuItems |> List.map (uncurry txtLink)
    buttonRow = buttons
              |> List.intersperse doubleDefSpacer
              |> (\x -> x ++ [doubleDefSpacer] )
              |> flow right
              |> centerHorizontally w
    content = flow down [ defaultSpacer
                         , divider purple1 w
                         , defaultSpacer
                         , buttonRow,
                         doubleDefSpacer ]
  in
    content |> container w (heightOf content) midTop