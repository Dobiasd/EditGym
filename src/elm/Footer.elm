module Footer where

import Graphics.Input (Input, input, customButton)
import Layout (defaultSpacer, gray1)

footer : Int -> Element
footer w =
  let
    main = plainText "Home" |> color gray1
    about = plainText "About" |> color gray1
    mainLink = customButton pageClick.handle MainPage main main main
    aboutLink = customButton pageClick.handle AboutPage about about about
    content = flow down [ defaultSpacer
                       , flow right [ mainLink
                                    , defaultSpacer
                                    , defaultSpacer
                                    , aboutLink ]
                       , defaultSpacer ]
  in
    content |> container w (heightOf content) midTop

pageClick : Input Page
pageClick = input MainPage

defaultPage : Page
defaultPage = MainPage

readPage : String -> Page
readPage s = if | s == "home" -> MainPage
                | s == "about" -> AboutPage
                | otherwise -> MainPage

showPageName : Page -> String
showPageName p =
  case p of
    MainPage -> "home"
    AboutPage -> "about"

data Page = MainPage | AboutPage