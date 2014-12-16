module Help where

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

# Introduction
After choosing a [level](?page=levels) your goal is to get the content of the editor window to match the content of the goal window by editing the text without using your mouse.
The number of key movements (up *and* down) is counted and there is also a stop watch.
When you completed a level you will see how good you were compared to other users and to create highscrore entries if you wish.
To achieve a good score you will need to be efficient at using some simple keyboard shortcuts.

# Shortcuts

## Move cursor
* arrow key left - move cursor left
* arrow key right - move cursor right
* arrow key up - move cursor one line up
* arrow key down - move cursor one line down
* ctrl+left - move cursor one word to the left
* ctrl+right - move cursor one word to the right
* pos1 - move cursor to beginning of line
* end - move cursor to end of line
* ctrl+pos1 - move cursor to beginning of document
* ctrl+end - move cursor to end of document

## Copy and paste
* ctrl+c or ctrl+insert - copy selected text
* ctrl+x or shift+delete - cut selected text
* ctrl+v or shift+insert - paste text from clipboard to cursor/selection

## Deleting
* ctrl+backspace - delete previous word
* ctrl+delete - delete next word
* ctrl+shift+backspace - delete current line to cursor
* ctrl+delete - delete current line from cursor on

## Selecting
* shift+any cursor movement - select text

### Examples
* shift+right - select character to the right
* shift+down - move one line down and select all skipped characters
* shift+end - select current line up to cursor
* shift+ctrl+right - select word to the right
* shift+ctrl+end - select everything starting at cursor position

## Typing
Of course you also can just type in the required text, but in most cases this is not the most efficient way.

## Reverting
* ctrl+z - undo last change
* ctrl+y - redo change


Now go and [work out your skills](?page=levels). :)

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