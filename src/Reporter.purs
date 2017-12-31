module Reporter where

import Prelude

import Control.Applicative
import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Either
import Data.Function
import Data.List hiding (init)
import Data.Maybe
import Data.Traversable (traverse)
import Data.Tuple

import Data.Argonaut
import Network.HTTP.Affjax (AJAX, get)

import DOM (DOM)

import Pux (CoreEffects, EffModel, start, noEffects, onlyEffects)
import Pux.DOM.Events (onClick, DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)

import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.HTML (button, div, span, input, h1, h2, li, ul, i, b, pre)
import Text.Smolder.Markup (text, (#!), (!))

import Url (serverUrl)

data Event = Interview
           | Publish (Either String String)

type Article = { headline :: String, date :: Int } 

type State =
  { newNews :: Article
  , oldNews :: List Article
  , errors :: List String
  }

init :: State
init = { newNews: { headline: "Bad Shit Happens", date: 0 }, oldNews: Nil, errors: Nil }

stp :: State -> String -> State
stp { newNews, oldNews, errors } str = { newNews: { headline: str, date: newNews.date + 1}, oldNews: (newNews:oldNews), errors }

requestComment :: forall fx. Aff (ajax :: AJAX | CoreEffects fx) (Maybe Event)
requestComment = do
  res <- attempt $ get serverUrl
  let decode r = decodeJson r.response :: Either String String
  let copy = either (Left <<< show) decode res
  pure $ Just $ Publish copy

foldp :: forall fx. Event -> State -> EffModel State Event (ajax :: AJAX | fx)
foldp (Publish (Right str)) st = noEffects $ stp st str
foldp (Publish (Left str)) st = noEffects $ st { errors = str:st.errors }
foldp Interview st = onlyEffects st $ [ requestComment ]

ouvre :: Article -> HTML Event
ouvre art = do
  li $ span $ do
    b $ text art.headline
    i $ text $ "Published: " <> show art.date

view :: State -> HTML Event
view state =
  div $ do
    div $ do
      h1 $ text "Breaking News"
      h2 $ text state.newNews.headline
    div $ do
      button #! onClick (const Interview) $ text "Interview Server"
    ul $ void $ traverse ouvre state.oldNews
    pre $ do
      void $ traverse (\x -> span $ text x) state.errors

main :: forall fx. Eff (ajax :: AJAX | CoreEffects fx) Unit
main = do
  app <- start { initialState: init
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
