module Newsletter where

import Window
import Signal

import UrlEncode (genLink)
import Layout(displayCoach)
import Graphics.Element (Element, flow, down, spacer)
import Layout(darkGray1, green1, white1, centerHorizontally, niceButton
  , defaultSpacer)
import Layout
import Skeleton

main : Signal Element
main = Signal.map2 scene Window.width Window.height

subscribeLink : String
subscribeLink = genLink "mailto:info@editgym.com " [("subject", "subscribe"), ("body", "Hi Coach,\nplease keep me informed about updates regarding EditGym.")]

unsubscribeLink : String
unsubscribeLink = genLink "mailto:info@editgym.com " [("subject", "unsubscribe"), ("body", "Hi coach,\nplease do not inform me about updates regarding EditGym any more.")]

scene : Int -> Int -> Element
scene w h =
  flow down [
      displayCoach "Do you me to keep you informed about updates like new exercises and stuff?\n Just use the subscribe button to send me an email. I will not send you any spam or something."
    , Layout.niceButton "subscribe" subscribeLink
    , spacer 1 100
    , displayCoach "You no longer want to receive updates? No problem, just use the unsubscribe button below."
    , Layout.niceButton "unsubscribe" unsubscribeLink
  ] |> Skeleton.showPage w h