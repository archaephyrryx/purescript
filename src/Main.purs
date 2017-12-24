module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Tuple
import Data.Maybe
import Data.List hiding (init)
import Data.Traversable (traverse)
import Pux (CoreEffects, EffModel, start, noEffects)
import Pux.DOM.Events (onClick, DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import DOM (DOM)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span, input)
import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.Markup (text, (#!), (!))
import Signal.Channel (CHANNEL)

data Event = InputChange DOMEvent
           | Click
           | Shout String

type State =
  { inp :: String
  , snaps :: List String
  }

init :: State
init = { inp: "", snaps: Nil }

announce :: forall f. String -> Eff (console :: CONSOLE, channel :: CHANNEL, exception :: EXCEPTION | f) Unit
announce = log


foldp :: forall fx. Event -> State -> EffModel State Event (console :: CONSOLE | fx)
foldp (InputChange ev) st = noEffects $ st { inp = targetValue ev }
foldp Click st@{ inp, snaps } = noEffects $ { inp: inp , snaps: (inp:snaps) }
foldp (Shout x) st = { state: st, effects: [ liftEff (announce x) *> pure Nothing ] }

view :: State -> HTML Event
view state =
  div $ do
    input ! value state.inp #! onChange InputChange
    button #! onClick (const Click) $ text "+"
    void $ traverse (\x -> button #! onClick (const $ Shout x) $ text x) state.snaps

main :: forall fx. Eff (console :: CONSOLE | CoreEffects fx) Unit
main = do
  app <- start { initialState: init
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
