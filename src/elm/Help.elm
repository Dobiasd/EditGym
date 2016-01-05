module Help (..) where

import Window
import Signal
import Markdown
import Graphics.Element exposing (Element, flow, down)
import Layout exposing (darkGray1, green1, white1, centerHorizontally, niceButton, defaultSpacer)
import Skeleton


exercisesButton : Element
exercisesButton =
    niceButton "Work out" "?page=exercises"


introduction : Int -> Element
introduction w =
    Skeleton.showTextPart w <| Markdown.toElement """

# Introduction
After choosing a [exercise](?page=exercises) your goal is to get the content of the editor window to match the content of the goal window by editing the text without using your mouse.
The number of key presses is counted and there also is a stop watch.
When you completed an exercise you will receive stars indicating the quality of your performance.
To achieve a good score you will need to be efficient at using some simple keyboard shortcuts.
Your personal bests will be saved.

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
* ctrl+shift+delete - delete current line from cursor on

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

## Indentation
Hitting the tabulator key produces four space characters.


Now go and [work out your skills](?page=exercises). :)

"""


main : Signal Element
main =
    Signal.map2 scene Window.width Window.height


scene : Int -> Int -> Element
scene w h =
    flow
        down
        [ introduction w
        , defaultSpacer
        , centerHorizontally w exercisesButton
        ]
        |> Skeleton.showPage w h
