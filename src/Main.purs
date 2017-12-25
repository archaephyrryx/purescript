module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION)

import Partial.Unsafe (unsafePartial)
import Data.Tuple
import Data.Maybe
import Data.List hiding (init)
import Data.Traversable (traverse)
import Pux (CoreEffects, EffModel, start, noEffects)
import Pux.DOM.Events (onClick, DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import DOM (DOM)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span, input, pre)
import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.Markup (text, (#!))--, (!))
import Signal.Channel (CHANNEL)

data Event = Toggle Scorp

type State = Bloop

type Scorp = { mx :: String, info :: List String, expand :: Boolean }
type Bloop = { query :: String, results :: List Scorp }

foodat :: Bloop
foodat = { query: "xyzzy.soy", results: ({ mx: "mail.xyzzy.soy", info: ("10.0.0.10":Nil), expand: false }:Nil) }

toggle :: State -> Scorp -> State
toggle s sco = s { results = unsafePartial $ toggle' sco s.results }
  where
    toggle' :: Partial => Scorp -> List Scorp -> List Scorp
    toggle' sco (x:xs)
      | x.mx == sco.mx = (sco { expand = not sco.expand }):xs
      | otherwise = x : toggle' sco xs

viewScorp :: Scorp -> HTML Event
viewScorp scorp = 
  div $ do
    span $ text scorp.mx
    div $ do
      button #! onClick (const $ Toggle scorp) $ text (if scorp.expand then "Collapse" else "Expand")
      if scorp.expand 
        then 
          pre $ void $ traverse (\x -> span $ text x) scorp.info
        else
          pure unit


view :: State -> HTML Event
view bloop =
  div $ do
    span $ text bloop.query
    void $ traverse viewScorp bloop.results


init :: State
init = foodat

{-
announce :: forall f. String -> Eff (console :: CONSOLE, channel :: CHANNEL, exception :: EXCEPTION | f) Unit
announce = log
-}


foldp :: forall fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (Toggle sco) st = noEffects $ toggle st sco

{-
view :: State -> HTML Event
view state =
  div $ do
    input ! value state.inp #! onChange InputChange
    button #! onClick (const Click) $ text "+"
    void $ traverse (\x -> button #! onClick (const $ Shout x) $ text x) state.snaps
-}

main :: forall fx. Eff (dom :: DOM | CoreEffects fx) Unit
main = do
  app <- start { initialState: init
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
