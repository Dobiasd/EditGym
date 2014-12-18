module FAQ where

import Window
import Signal

import Markdown

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

exercisesButton : Element
exercisesButton = niceButton "Work out" "?page=exercises"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

# EditGym - FAQ

## Are you planning to support modes for emacs or vi(m)?
No, this is not planned at the moment.

## Why are some keys disabled for typing?
Only the keys needed for the [default exercises](?page=exercises) are activated.

## Why is the text selection displayed so ugly?
Normally it should look fine. But since it is generated using an [UTF-8 character](http://www.fileformat.info/info/unicode/char/2588/index.htm), your browser has to support a nice monospace font.

## Why can I not copy and paste from or into the text editor in the exercises?
The editor simply does not use the clipboard provided by the operating system but it's owm implementation.

## Why can I not use my mouse to select text?
Is is disabled because this page was build to make you a keyboarding master, not a mouse fool.

## Why does holding down a key not produce many characters like usually?
You are here to learn efficient shortcuts. Solving a exercise by holding down a key would reinforce inefficient habits.

## Why does this site not scale correctly onto my tiny screen?
At the default zoom level the layout was made for screen resolutions with at least 1280 pixels in width. But you can always just zoom in and zoom out with ctrl + plus / ctrl + minus / ctrl + mouse wheel.

## Why does EditGym not behave like my favorite text editor?
In some cases not all editors behave the same.

Take the following scenario as an example:

```
word1 word2
```

Your cursor is at the beginning of word1 (left of the w) and you press
`ctrl+right`
* Libreoffice Writer (4.2.7.2) will jump to the beginning of the next word.
* Notepad++ (6.3.3) will jump to the beginning of the next word.
* Sublime Text (3, build 3065) will jump to the end of the current word.

But if you know add `shift` (i.e. press `ctrl+shift+right`) you get this:
* Libreoffice Writer (4.2.7.2) will mark to the beginning of the next word.
* Notepad++ (6.3.3) will mark to the end of the current word.
* Sublime Text (3, build 3065) will mark to the end of the current word.

---

Or consider this:

```
line1
line2
line3
```

Your cursor is at the beginning of line 3 (left of the l) and you press
`ctrl+shift+backspace`
* Libreoffice Writer (4.2.7.2) will remove the whole line above the cursor.
* Notepad++ (6.3.3) will do nothing at all.
* Sublime Text (3, build 3065) will remove only the newline character between line 2 and line 3.
So it is not possible to cover everything. [I](?page=contact) opted for the behavior that subjectively seemed to be the most sane and woult not teach habits not applicable when switching between editors. So in this particular case I have chosen "do nothing at all".

## I have a suggestion for an awesome exercise. Can you include it?
Yes, perhaps. Just [send me an email](?page=contact) with the start and goal text, and I will see if I think it fits here. ;)
In any case, you can always simply [create your own exercise](?page=create_exercise).

## How was this page made?
This page was mostly written in [Elm](http://elm-lang.org), an awesome [FRP](http://en.wikipedia.org/wiki/Functional_reactive_programming)-utilizing [pure functional](http://en.wikipedia.org/wiki/Functional_programming) programming language that compiles to Javascript.

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w exercisesButton ]
  |> Skeleton.showPage w h