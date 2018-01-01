module Main where

import Prelude hiding (div)

import Control.Applicative
import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except

import Data.Array as A
import Data.Array hiding (span, (:), cons, init, (..))
import Data.Bifunctor (bimap)
import Data.Either
import Data.Function
import Data.Foreign (ForeignError, renderForeignError)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode, genericDecodeJSON, genericEncodeJSON)
import Data.Generic.Rep
import Data.List as L
import Data.List.NonEmpty (toList, NonEmptyList)
import Data.List hiding (span, uncons, init, (..))
import Data.Maybe
import Data.Traversable (traverse)
import Data.Tuple

import Optic.Core hiding (div, view)
import Optic.Lens

import Partial.Unsafe (unsafePartial)

import Data.Argonaut
import Network.HTTP.Affjax (AJAX, get)

import DOM (DOM)

import Pux (CoreEffects, EffModel, start, noEffects, onlyEffects)
import Pux.DOM.Events (onClick, DOMEvent, onChange, targetValue, onSubmit)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Text.Smolder.HTML hiding (map)
import Text.Smolder.HTML.Attributes hiding (span, form)
import Text.Smolder.Markup

import Signal.Channel (CHANNEL)

import Url (serverUrl)

data Event = Submit
           | InputChange DOMEvent
           | Toggle Detail
           | Remote (Either (List String) Response)
           | Error (List String)
           | Update Response

type State = { inp :: String, resp :: Response, err :: List String }

type Response = Maybe Answer
type RawResponse = Maybe RawAnswer

newtype Answer = Answer { query :: String, results :: Array Detail }
newtype Detail = Detail { mx :: String, info :: Array String, expand :: Boolean }

_Answer :: Lens' Answer { query :: String, results :: Array Detail }
_Answer = lens (\(Answer x) -> x) (const Answer)

_Detail :: Lens' Detail { mx :: String, info :: Array String, expand :: Boolean } 
_Detail = lens (\(Detail x) -> x) (const Detail)

query :: Lens' { query :: String, results :: Array Detail } String
query = lens _.query (_ { query = _ })

results :: Lens' { query :: String, results :: Array Detail } (Array Detail)
results = lens _.results (_ { results = _ })

mx :: Lens' { mx :: String, info :: Array String, expand :: Boolean } String
mx = lens _.mx (_ { mx = _ })

info :: Lens' { mx :: String, info :: Array String, expand :: Boolean } (Array String)
info = lens _.info (_ { info = _ })

expand :: Lens' { mx :: String, info :: Array String, expand :: Boolean } Boolean
expand = lens _.expand (_ { expand = _ })

newtype RawAnswer = RawAnswer { query :: String, results :: Array RawDetail }
newtype RawDetail = RawDetail { mx :: String, info :: Array String }

derive instance genericDetail    :: Generic Detail    _
derive instance genericRawDetail :: Generic RawDetail _
derive instance genericAnswer    :: Generic Answer    _
derive instance genericRawAnswer :: Generic RawAnswer _

opts = defaultOptions { unwrapSingleConstructors = true }

instance decodeRawAnswer :: Decode RawAnswer where
  decode = genericDecode opts
instance decodeRawDetail :: Decode RawDetail where
  decode = genericDecode opts

noAnswer :: Response
noAnswer = Nothing

init :: State
init = { inp: "", resp: noAnswer, err: Nil }

annotate :: RawAnswer -> Answer
annotate (RawAnswer { query, results }) = Answer { query: query, results: tag <$> results }
  where
    tag :: RawDetail -> Detail
    tag (RawDetail { mx, info }) = Detail { mx: mx, info: info, expand: false }



one :: forall a. a -> List a
one x = x:Nil

resolve :: forall fx. String -> Aff (ajax :: AJAX | CoreEffects fx) (Maybe Event)
resolve q = do
  res <- attempt $ get (serverUrl <> q)
  let decode r = runExcept (genericDecodeJSON opts r.response) :: Either (NonEmptyList ForeignError) RawResponse
  let copy = either (Left <<< one <<< show) (bimap (toList <<< map renderForeignError) (map annotate) <<< decode) res
  pure $ Just $ Remote $ copy

similar :: forall a b. Eq b => (a -> b) -> a -> a -> Boolean
similar f x y = f x == f y

toggle :: Detail -> Answer -> Answer
toggle det ans = ans # _Answer..results %~ safeToggle det
  where
    safeToggle :: Detail -> Array Detail -> Array Detail
    safeToggle det dets = unsafePartial $ toggle' det dets
    toggle' :: Partial => Detail -> Array Detail -> Array Detail
    toggle' det dets = case uncons dets of
      Just { head: x, tail: xs } | similar (\x -> x ^. _Detail..mx) x det -> A.cons (det # _Detail..expand %~ not) xs
                                 | otherwise                          -> A.cons x $ toggle' det xs

expandCollapse :: Detail -> HTML Event
expandCollapse det = do
    button #! onClick (const $ Toggle det) $ text (excolText $ det ^. _Detail..expand)
  where
    excolText true = "◀︎"
    excolText false = "▶︎"

viewDetail :: Detail -> HTML Event
viewDetail det =
  div $ do
    div $ do
      p $ do
        expandCollapse det
        span $ text $ det ^. _Detail..mx
      if det ^. _Detail..expand
        then
          pre $ void $ traverse (\x -> span $ text x) $ det ^. _Detail..info
        else
          pure unit

viewResponse :: Response -> HTML Event
viewResponse Nothing = span $ text "No domains matching query."
viewResponse (Just ans) = do
  span $ text $ ans ^. _Answer..query
  void $ traverse viewDetail $ ans ^. _Answer..results

view :: State -> HTML Event
view stat =
  div $ do
    form ! name "search" #! onSubmit (const Submit) $ do
      input ! type' "text" ! value stat.inp #! onChange InputChange
      button ! type' "submit" $ text "Go"
    pre $ void $ traverse (\x -> span $ text x) stat.err
    div $ viewResponse stat.resp

foldp :: forall fx. Event -> State -> EffModel State Event (dom :: DOM, ajax :: AJAX | fx)
foldp (InputChange ev) st = noEffects $ st { inp = targetValue ev }
foldp Submit st = onlyEffects st $ [ resolve st.inp ]
foldp (Toggle det) st = noEffects $ st { resp = toggle det <$> st.resp }
foldp (Remote (Left strs)) st = onlyEffects st $ [ pure $ Just $ Error strs ]
foldp (Remote (Right resp)) st = onlyEffects st $ [ pure $ Just $ Update resp ]
foldp (Error strs) st = noEffects $ st { err = strs<>st.err }
foldp (Update resp) st = noEffects $ update resp st
  where
    update resp st = st { resp = resp, err = Nil }

main :: forall fx. Eff (ajax :: AJAX, dom :: DOM | CoreEffects fx) Unit
main = do
  app <- start { initialState: init
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
