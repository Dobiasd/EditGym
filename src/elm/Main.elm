module Main where

import Keyboard
import Set
import Window

import Editor
import KeyHistory
import Skeleton (showPage)

import Start
import Levels
import Game

-- Can not reference Start.click.signal.
-- Thus it is named Start.startlick, so it can be imported directly.
import Start(startClick)
import Levels(levelClick)
import Page

type State = {
    page : Page.Page
  , game : Game.State
  }

type Input = { inKeysDown : Set.Set Int, page : Page.Page }

pageChange : Signal Page.Page
pageChange = dropIf (\s -> s == Page.NoPage) Page.Start
  <| merges [startClick.signal, Page.Game <~ levelClick.signal]

input : Signal Input
input = Input
  <~ ((Set.fromList << filter validKey) <~ Keyboard.keysDown)
  ~ pageChange

validKey : Int -> Bool
validKey key =
  if | key >= 65 && key <= 90 -> True -- a to z
     | key >= 37 && key <= 40 -> True -- arrow keys
     | key == 16 -> True -- shift
     | key == 17 -> True -- ctrl
     | key == 32 -> True -- space
     | key >= 48 && key <= 57 -> True -- top numbers
     | key >= 96 && key <= 105 -> True -- block numbers
     | key == 13 -> True -- enter
     -- | key == 33 || key == 34 -> True -- page up, page down
     | key == 35 || key == 36 -> True -- end, pos1
     | key == 106 || key == 107 || key == 109 -> True -- *, +, -
     | key == 188 || key == 190 -> True -- , and .
     | key == 46 -> True -- delete
     | key == 8 -> True -- backspace
     | otherwise -> True

initialState : State
initialState =
  State
    Page.Start
    Game.initialState

state : Signal State
state = foldp step initialState input

step : Input -> State -> State
step input ({game} as state) =
  { state | game <- Game.step (Game.Input input.inKeysDown) game
          , page <- input.page }

main : Signal Element
main = scene <~ Window.width ~ state

scene : Int -> State -> Element
scene w {game, page} =
  case page of
    Page.Start -> Start.display w
    Page.Levels -> Levels.display w
    Page.Game level -> Game.display w game