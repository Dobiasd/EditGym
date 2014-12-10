module Footer where

import Layout (defaultSpacer, gray1)

footer : Int -> Element
footer w =
  let
    txtLink str url = plainText str |> color gray1 |> link url
    start = txtLink "Home" "?page=start"
    highscore = txtLink "Highscore list" "?page=highscores"
    faq = txtLink "FAQ""?page=faq"
    levels = txtLink "Levels" "?page=levels"
    help = txtLink "Help" "?page=help"
    contact = txtLink "Contact" "?page=contact"
    content = flow down [ defaultSpacer
                       , flow right [ start
                                    , defaultSpacer, defaultSpacer
                                    , levels
                                    , defaultSpacer, defaultSpacer
                                    , highscore
                                    , defaultSpacer, defaultSpacer
                                    , help
                                    , defaultSpacer, defaultSpacer
                                    , faq
                                    , defaultSpacer, defaultSpacer
                                    , contact ]
                       , defaultSpacer ]
  in
    content |> container w (heightOf content) midTop