module Skeleton where

import Layout (defaultSpacer, pageWidth, toDefText, toSizedText,
  black1, white1, orange1, blue1, purple1, red1, green1, gray1)
import Footer (footer)
import Header (header)

showPage : Int -> Int -> Element -> Element
showPage w h content =
  let
    content' = content |> container w (heightOf content) midTop
    headerElem = header w
    footerElem = footer w
    pageH = heightOf headerElem + heightOf content + heightOf footerElem + 6
    divider = flow down [ spacer 1 4 |> color black1
                        , spacer w 3 |> color orange ]
                        |> container w 7 midTop
    pageContentHeight = sum [
        heightOf headerElem
      , heightOf content'
      , heightOf divider
      , heightOf footerElem
      ]
    h' = max h pageH
    footerSpacer = spacer 2 <| h' - pageContentHeight
  in
    flow down [
      headerElem
    , content'
    , footerSpacer
    , divider
    , footerElem
    ] |> color black1 |> container w (max h h') midTop