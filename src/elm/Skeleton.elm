module Skeleton where

import Layout (defaultSpacer, pageWidth, toDefText, toSizedText,
  black1, white1, orange1, blue1, purple1, red1, green1, gray1)
import Footer (footer)
import Header (header)

showPage : Int -> Element -> Element
showPage w content =
  let
    headerElem = header w
    footerElem = footer w
    h = heightOf headerElem + heightOf content + heightOf footerElem + 6
    divider = flow down [ spacer 1 4 |> color black1
                        , spacer w 3 |> color orange ]
                        |> container w 7 midTop
  in
    flow down [
      headerElem
    , content
    , divider
    , footerElem
    ] |> color black1 |> container w h midTop