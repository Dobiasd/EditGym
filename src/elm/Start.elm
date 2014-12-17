module Start where

import Window
import Signal

import Markdown

import Graphics.Element (Element, flow, down, right, spacer)
import Skeleton
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)

startButton : Element
startButton = niceButton "Start" "?page=exercises"

helpButton : Element
helpButton = niceButton "Help" "?page=help"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

Sometimes you need to edit or rearrange text quickly. No matter if you use a [word processor](https://www.libreoffice.org/discover/writer), an [editor](http://notepad-plus-plus.org), an [IDE](https://eclipse.org)  or just write an [email](https://www.mozilla.org/en-US/thunderbird/) or a post on [facebook](http://www.facebook.com), the basic keyboard shortcuts are mostly the same.

But [just memorizing them](https://www.shortcutfoo.com) alone is not enough to really [save time](http://acrobolix.com/keyboarding-changed-my-life/).
It is also important (and sometimes even fun) to choose the most efficient tactics effortlessly.

EditGym helps you to acquire this set of skills by letting you practice and compete on standardized text snippets.

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w (flow right [ startButton
                                     , spacer 40 1
                                     , helpButton ])
  ]
  |> Skeleton.showPage w h