module Skeleton (..) where

import Graphics.Element exposing (Element, container, topRight, width, heightOf, fittedImage, middle, color, midTop, flow, down, spacer, outward, topLeft, empty)
import List
import Layout exposing (defaultSpacer, toDefText, toSizedText, white1, orange1, blue1, purple1, red1, green1, gray1, divider, darkGray1, centerHorizontally)
import Footer exposing (footer)
import Header exposing (header)


showTextPart : Int -> Element -> Element
showTextPart w content =
    let
        content' = content |> width 720

        h = (heightOf content')

        img = fittedImage w h "imgs/keyboard_bg.jpg"
    in
        flow outward [ img, content' |> centerHorizontally w ]


showPage : Int -> Int -> Element -> Element
showPage wFull h content =
    let
        w = wFull - 4

        -- prevent scrollbars from flickering when zoomed in browser
        content' = content |> container w (heightOf content) midTop

        headerElem = header w

        footerElem = footer w

        pageH = List.map heightOf [ headerElem, content', footerElem ] |> List.sum

        h' = max h pageH - 4

        footerSpacer = spacer 2 <| max 0 (h' - pageH)
    in
        flow
            down
            [ headerElem
            , content'
            , footerSpacer
            , footerElem
            ]
            |> container w (max h h') topLeft
