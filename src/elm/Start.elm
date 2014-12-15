module Start where

import Window
import Signal

import Markdown

import Graphics.Element (Element, flow, down, right, spacer)
import Skeleton
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)

startButton : Element
startButton = niceButton "Start" "?page=levels"

helpButton : Element
helpButton = niceButton "Help" "?page=help"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

A lot of daily tasks require you to edit text. And no matter if you use a word processor like [LibreOffice Writer](https://www.libreoffice.org/discover/writer) or [Word](http://en.wikipedia.org/wiki/Microsoft_Word), an editor like [Notepad(++)](http://notepad-plus-plus.org), an [IDE](http://en.wikipedia.org/wiki/Integrated_development_environment) like [Eclipse](https://eclipse.org) etc. or just write an email or a post on [facebook](http://www.facebook.com) or [reddit](http://www.reddit.com) in your browser, the most basic default keyboard shortcuts are the same. And knowing them can [save you precious time](http://lifehacker.com/5970089/back-to-the-basics-learn-to-use-keyboard-shortcuts-like-a-ninja).
But real efficiency is not reached by [just memorizing all shortcuts](https://www.shortcutfoo.com). It is also important (and fun!) to be able to effortlessly choose the tactics with the smallest amount of shortcuts to reach the desired goal.
EditGym helps you acquire this set of skills by letting you practice and compete on standardized text snippets.

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