module Skeleton where

import Graphics.Element (Element, container, topRight, width, heightOf
  , fittedImage, middle, color, midTop, flow, down, spacer)
import Graphics.Collage (collage, toForm)
import List
import Layout (defaultSpacer, pageWidth, toDefText, toSizedText
  , black1, white1, orange1, blue1, purple1, red1, green1, gray1, divider
  , darkGray1)
import Footer (footer)
import Header (header)

showTextPart : Int -> Element -> Element
showTextPart w content =
  let content' = content |> width 640
      h = (heightOf content')
      img = fittedImage w h "imgs/keyboard_bg.jpg"
      row = collage w h [toForm img, toForm content']
  in  container w h middle row |> color white1

showPage : Int -> Int -> Element -> Element
showPage w h content =
  let
    content' = content |> container w (heightOf content) midTop
    headerElem = header w
    footerElem = footer w
    pageH = List.map heightOf [ headerElem, content, footerElem ] |> List.sum

    pageContentHeight = List.sum [
        heightOf headerElem
      , heightOf content'
      , heightOf footerElem
      ]
    h' = max h pageH
    footerSpacer = spacer 2 <| h' - pageContentHeight
  in
    flow down [
      headerElem
    , content'
    , footerSpacer
    , footerElem
    ] |> color black1 |> container w (max h h') midTop