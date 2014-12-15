module Footer where

import Layout (defaultSpacer, gray1, toDefText)
import Graphics.Element (Element, container, heightOf, widthOf, middle
    , color, link, flow, down, right, midTop)
import Color (Color)

txtLink str url =
    let elem = toDefText str
    in  container (widthOf elem + 10) (heightOf elem + 6) middle elem
        |> color gray1 |> link url

footer : Int -> Element
footer w =
  let
    start = txtLink "Home" "?page=start"
    highscore = txtLink "Highscore list" "?page=highscores"
    faq = txtLink "FAQ""?page=faq"
    levels = txtLink "Levels" "?page=levels"
    createLevel = txtLink "Create level" "?page=create_level"
    help = txtLink "Help" "?page=help"
    contact = txtLink "Contact" "?page=contact"
    content = flow down [ defaultSpacer
                       , flow right [ start
                                    , defaultSpacer, defaultSpacer
                                    , levels
                                    , defaultSpacer, defaultSpacer
                                    , highscore
                                    , defaultSpacer, defaultSpacer
                                    , createLevel
                                    , defaultSpacer, defaultSpacer
                                    , help
                                    , defaultSpacer, defaultSpacer
                                    , faq
                                    , defaultSpacer, defaultSpacer
                                    , contact ]
                       , defaultSpacer ]
  in
    content |> container w (heightOf content) midTop