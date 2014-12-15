module FAQ where

import Window
import Signal

import Markdown

import Graphics.Element (Element, flow, down)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Skeleton

levelsButton : Element
levelsButton = niceButton "Work out" "?page=levels"

introduction : Int -> Element
introduction w = Skeleton.showTextPart w <| Markdown.toElement """

# EditGym - FAQ

## Are you planning to support modes for emacs or vi(m)?
No, this is not planned at the moment.

## Why are some keys disabled for typing?
Only the keys needed for the [default levels](?page=levels) are activated.

## Why are the cursor and the selection displayed so ugly?
Normally they should look fine. But since they are generated via UTF-8 characters, your browser has to support a nice monospace font containing symbols for these characters.

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
So it is not possible to cover everything. [I](?page=contact) opted for the behavior that subjectively seemed to be the most sane and will teach habits applicable to most editors.

## I have a suggestion for an awesome level. Can you include it?
Yes, perhaps. Just [send me an email](?page=contact) with the start and goal text, and I will see if I think it fits here. ;)
In any case, you can simply [upload it and play it here](?page=create_level).

## How was this page made?
This page was mostly written in [Elm](http://elm-lang.org), an awesome [pure functional](http://en.wikipedia.org/wiki/Functional_programming) [Haskell](http://www.haskell.org)-like programming language that compiles to Javascript.

## Can you show me the source code?
Sure! Here it is: [github/Dobiasd/EditGym](https://github.com/Dobiasd/EditGym)

"""

main : Signal Element
main = Signal.map2 scene Window.width Window.height

scene : Int -> Int -> Element
scene w h =
  flow down [
    introduction w
  , defaultSpacer
  , centerHorizontally w levelsButton ]
  |> Skeleton.showPage w h