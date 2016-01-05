module Footer (..) where

import List
import Text
import Graphics.Element exposing (Element, container, heightOf, widthOf, middle, color, link, flow, down, right, midTop, spacer, rightAligned)
import Layout exposing (defaultSpacer, darkGray1, toDefText, divider, purple1, centerHorizontally, quadDefSpacer, lightGray1)
import Header exposing (menu)


txtLink str url =
    toDefText str |> link url


menuItems : List ( String, String )
menuItems =
    [ ( "Help", "?page=help" )
    , ( "FAQ", "?page=faq" )
    , ( "Contact", "?page=contact" )
    ]


footer : Int -> Element
footer w =
    let
        buttons = menuItems |> menu quadDefSpacer

        copyright =
            "Copyright © 2014 Tobias Hermann. All rights reserved."
                |> Text.fromString
                |> Text.height 12
                |> Text.color lightGray1
                |> rightAligned

        copyRightElem = flow right [ copyright, defaultSpacer ]

        leftSpacerW = (w - widthOf buttons) // 2

        rightSpacerW = leftSpacerW - widthOf copyright

        content =
            flow
                down
                [ defaultSpacer
                , divider purple1 w
                , defaultSpacer
                , flow
                    right
                    [ spacer leftSpacerW 1
                    , buttons
                    , spacer rightSpacerW 1
                    , copyRightElem
                    ]
                ]
    in
        content |> container w (heightOf content) midTop
