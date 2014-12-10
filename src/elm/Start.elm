module Start where

import Window

import Skeleton
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)

startButton : Element
startButton = niceButton "Start" "?page=levels"

helpButton : Element
helpButton = niceButton "Help" "?page=help"

introduction : Int -> Element
introduction w =
  let content = [markdown|

A lot of daily tasks require you to edit text. And no matter if you use a word processor like [LibreOffice Writer](https://www.libreoffice.org/discover/writer) or [Word](http://en.wikipedia.org/wiki/Microsoft_Word), an editor like [Notepad(++)](http://notepad-plus-plus.org) or [Sublime](http://www.sublimetext.com), an [IDE](http://en.wikipedia.org/wiki/Integrated_development_environment) like [Eclipse](https://eclipse.org) or [Visual Studio](http://www.visualstudio.com) etc. or just write an email or a post on [facebook](http://www.facebook.com), [reddit](http://www.reddit.com) or [stackoverflow](http://stackoverflow.com) in your browser, the most basic default keyboard shortcuts are the same. And knowing them can [save you precious time](http://lifehacker.com/5970089/back-to-the-basics-learn-to-use-keyboard-shortcuts-like-a-ninja).
But real efficiency is not reached by [just memorizing all shortcuts](https://www.shortcutfoo.com). It is also important (and fun!) to be able to effortlessly choose the tactics with the smallest amount of shortcuts to reach the desired goal.
EditGym.com helps you acquire this set of skills.

|] |> width 640
      img = image 800 280 "imgs/keyboard_bg.jpg"
      row = collage 800 280 [toForm img, toForm content]
  in  container w 280 middle row |> color white1

main : Signal Element
main = scene <~ Window.width ~ Window.height

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