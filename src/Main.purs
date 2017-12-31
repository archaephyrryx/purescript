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
import Text.Smolder.HTML (button, div, span, input, pre, p)
import Text.Smolder.HTML.Attributes (value, type')
import Text.Smolder.Markup (text, (#!), (!))
import Signal.Channel (CHANNEL)

data Event = Push
           | InputChange DOMEvent
           | Toggle Scorp

type State = { inp :: String, now :: Bloop }

type Scorp = { mx :: String, info :: List String, expand :: Boolean }
type Bloop = { query :: String, results :: List Scorp }

resolve :: String -> Bloop
resolve x = resolve' x samples
  where
    resolve' x (s:rest) | x == s.query = s
                        | otherwise = resolve' x rest
    resolve' x (Nil) = noSuch

noSuch :: Bloop
noSuch = { query: "this is not a domain...", results : Nil }

samples :: List Bloop
samples = querify "xyzzy.soy" 10 : querify "yargle.blarg" 20 : querify "dane.sys4.de" 77 : Nil

foodat :: Bloop
foodat = querify "xyzzy.soy" 10

querify :: String -> Int -> Bloop
querify q ip =
  let i = show ip
   in { query: q, results :({ mx: ("mail."<>q), info: ((i<>".0.0."<>i):Nil), expand: false}:Nil) }

toggle :: Bloop -> Scorp -> Bloop
toggle s sco = s { results = unsafePartial $ toggle' sco s.results }
  where
    toggle' :: Partial => Scorp -> List Scorp -> List Scorp
    toggle' sco (x:xs)
      | x.mx == sco.mx = (sco { expand = not sco.expand }):xs
      | otherwise = x : toggle' sco xs


expandCollapse :: Scorp -> HTML Event
expandCollapse sco = do
    button #! onClick (const $ Toggle sco) $ text (excolText sco.expand)
  where
    excolText true = "◀︎"
    excolText false = "▶︎"

viewScorp :: Scorp -> HTML Event
viewScorp scorp =
  div $ do
    div $ do
      p $ do
        expandCollapse scorp
        span $ text scorp.mx
      if scorp.expand
        then
          pre $ void $ traverse (\x -> span $ text x) scorp.info
        else
          pure unit


view :: State -> HTML Event
view stat =
  div $ do
    div $ do
      input ! type' "text" ! value stat.inp #! onChange InputChange
      button #! onClick (const $ Push) $ text "Go"
    div $ do
      span $ text stat.now.query
      void $ traverse viewScorp stat.now.results


init :: State
init = { inp: "", now: noSuch }

{-
announce :: forall f. String -> Eff (console :: CONSOLE, channel :: CHANNEL, exception :: EXCEPTION | f) Unit
announce = log
-}


foldp :: forall fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (InputChange ev) st = noEffects $ st { inp = targetValue ev }
foldp (Push) st = noEffects $ st { now = resolve st.inp }
foldp (Toggle sco) st = noEffects $ st { now = toggle st.now sco }

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
