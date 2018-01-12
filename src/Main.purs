module Main where

import Prelude hiding (div, one)

import Control.Applicative
import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, Ref, REF, readRef, writeRef)
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
import Optic.Core as OL
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
import Text.Smolder.HTML.Attributes hiding (span, form, id)
import Text.Smolder.Markup

import Signal.Channel (CHANNEL)

import Url (serverUrl)
import Comms
import Model

type AppEffects = (ajax :: AJAX, ref :: REF, dom :: DOM)

data Event = Submit
           | Cancel
           | Cancelled
           | Clear
           | Cleared
           | InputChange DOMEvent
           | Remote (Either (List String) Response)
           | Pester String
           | FollowUp (Either (List String) RawDetail)
           | Toggle Detail
           | Error (List String)
           | Await String
           | Listening String
           | Update Response
           | MXUpdate RawDetail

type Trigger = Fiber (CoreEffects AppEffects) (Maybe Event)

type State = { inp :: String, resp :: Response, queue :: List String, waiting :: Boolean, err :: List String, trigger :: Ref (Maybe Trigger) }

init :: (Ref (Maybe Trigger)) -> State
init trig = { inp: "", resp: noAnswer, queue: Nil, waiting: false, err: Nil, trigger: trig }

cancel :: Ref (Maybe Trigger) -> Aff (CoreEffects AppEffects) Unit
cancel trig =
  liftEff (readRef trig) >>= maybe (pure unit) (killFiber (error "late"))

clearRef :: forall x fx. Ref (Maybe x) -> Eff (ref :: REF | fx) Unit
clearRef ref = writeRef ref Nothing

resolve :: (String -> Aff (CoreEffects AppEffects) (Maybe Event)) -> String -> Ref (Maybe Trigger) -> Aff (CoreEffects AppEffects) (Maybe Event)
resolve f q trig = do
  fib <- forkAff $ f q
  liftEff $ writeRef trig (Just fib)
  res <- try (joinFiber fib)
  pure $ either (const Nothing) id res

domainName :: State -> String
domainName st = maybe (st.inp) (\x -> x ^. _Answer..query) st.resp

getHost :: String -> Aff (CoreEffects AppEffects) (Maybe Event)
getHost q = do
  res <- attempt $ get (serverUrl <> q)
  let copy = either (Left <<< one <<< show) (orError (Just <<< annotate) <<< parseJSON) res
  pure $ Just $ Remote $ copy

getMX :: String -> String -> Aff (CoreEffects AppEffects) (Maybe Event)
getMX dom q = do
  res <- attempt $ get (serverUrl <> dom <> "/" <> q)
  let copy = either (Left <<< one <<< show) (orError id <<< parseJSON) res
  pure $ Just $ FollowUp $ copy

toggle :: Detail -> Answer -> Answer
toggle det ans = ans # _Answer..results %~ safeToggle det
  where
    safeToggle :: Detail -> Array Detail -> Array Detail
    safeToggle det dets = unsafePartial $ toggle' det dets
    toggle' :: Partial => Detail -> Array Detail -> Array Detail
    toggle' det dets = case uncons dets of
      Just { head: x, tail: xs } | similar (\x -> x ^. _Detail..mx) x det -> A.cons (det # _Detail..expand %~ not) xs
                                 | otherwise                              -> A.cons x $ toggle' det xs

expound :: RawDetail -> Answer -> Answer
expound rd ans = ans # _Answer..results %~ safeExpound rd
  where
    safeExpound :: RawDetail -> Array Detail -> Array Detail
    safeExpound rdet dets = unsafePartial $ expound' rdet dets
    expound':: Partial => RawDetail -> Array Detail -> Array Detail
    expound' rdet@(RawDetail { mx: m, info: i }) dets = case uncons dets of
      Just { head: x, tail: xs } | (x ^. _Detail..mx.._MxInfo..hostname) == m -> A.cons (x # _Detail..info .~ i) xs
                                 | otherwise -> A.cons x $ expound' rdet xs

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
        span $ text $ getHN det
      if det ^. _Detail..expand
        then
          pre $ void $ traverse (\x -> span $ text x) $ det ^. _Detail..info
        else
          pure unit

viewResponse :: Response -> HTML Event
viewResponse Nothing = span $ text "Enter a domain to retrieve results for."
viewResponse (Just ans) = do
  span $ text $ ans ^. _Answer..query
  void $ traverse viewDetail $ ans ^. _Answer..results

view :: State -> HTML Event
view stat =
  div $ do
    div $ do
      input ! type' "text" ! value stat.inp #! onChange InputChange
      button ! type' "submit" #! onClick (const Submit) $ text "Go"
      button #! onClick (const Cancel) $ text "Cancel"
    pre $ void $ traverse (\x -> span $ text x) stat.err
    if stat.waiting
      then do
        span $ text $ "Retrieving results for domain "<>stat.inp<>" (please wait...)"
        span $ text "(If nothing changes the page isn't necessarily frozen)"
      else
        div $ viewResponse stat.resp

waitOn :: State -> Aff (CoreEffects AppEffects) (Maybe Event)
waitOn st = pure $ Just $ Await st.inp

listenFor :: String -> Aff (CoreEffects AppEffects) (Maybe Event)
listenFor str = pure $ Just $ Listening str


{-
 - this function is used to update the state to indicate that the
 - results for a given mx host (the string argument) are being listened
 - for, but is currently a no-op because our model does not have any kind
 - of indicator for this state currently. XXX Make this do something at
 - some point
 -}
listen :: State -> String -> State
listen = const

update :: Response -> State -> State
update resp st = st { resp = resp, waiting = false, err = Nil, queue = requeue resp }

getHN :: Detail -> String
getHN x = x ^. _Detail..mx.._MxInfo..hostname

requeue :: Response -> List String
requeue Nothing = Nil
requeue (Just x) = A.toUnfoldable $ map getHN $ x ^. _Answer..results

mxupdate :: RawDetail -> State -> State
mxupdate rawd st = st { resp = (expound rawd <$> st.resp), waiting = false, err = Nil }

matchMX :: RawDetail -> (Detail -> Boolean)
matchMX = ((>>>) getHN) <<< ((==) <<< getmx)

repester :: State -> RawDetail -> Aff (CoreEffects AppEffects) (Maybe Event)
repester stat rawd =
  let rem = complete rawd stat.queue
   in pure $ (Pester <<< _.head) <$> L.uncons rem

complete :: RawDetail -> List String -> List String
complete rawd = L.filter (\x -> x /= getmx rawd)

foldp :: Event -> State -> EffModel State Event (AppEffects)
foldp (InputChange ev) st = noEffects $ st { inp = targetValue ev }
foldp Cancelled st = noEffects $ st { waiting = false }
foldp (Toggle det) st = noEffects $ st { resp = toggle det <$> st.resp }
foldp (Error strs) st = noEffects $ st { err = strs<>st.err, waiting = false }
foldp (MXUpdate rawd) st = noEffects $ st { resp = expound rawd <$> st.resp, queue = complete rawd st.queue }
foldp (Await str) st = noEffects $ st { waiting = true }
foldp (Listening str) st = noEffects $ listen st str
foldp (Update resp) st =
  let st' = update resp st
   in { state: st', effects: [ pure $ (Pester <<< _.head) <$> L.uncons st'.queue ] }
foldp (Pester str) st =
  onlyEffects st [ resolve (getMX $ domainName st) str st.trigger
                 , listenFor str
                 ]
foldp Submit st =
  onlyEffects st [ pure $ Just Clear ]
foldp Cleared st =
  onlyEffects st [ resolve getHost st.inp st.trigger
                 , waitOn st
                 ]
foldp Cancel st =
  onlyEffects st [ cancel st.trigger *> pure Nothing
                 , liftEff (clearRef st.trigger) *> pure Nothing
                 , pure $ Just $ Cancelled
                 ]
foldp Clear st =
  onlyEffects st [ cancel st.trigger *> pure Nothing
                 , liftEff (clearRef st.trigger) *> pure Nothing
                 , pure $ Just $ Cleared
                 ]
foldp (Remote (Left strs)) st =
  onlyEffects st [ pure $ Just $ Error strs ]
foldp (FollowUp (Left strs)) st =
  onlyEffects st [ pure $ Just $ Error strs ]
foldp (Remote (Right resp)) st =
  onlyEffects st [ pure $ Just $ Update resp
                 , liftEff (clearRef st.trigger) *> pure Nothing
                 ]
foldp (FollowUp (Right rawd)) st =
  onlyEffects st [ pure $ Just $ MXUpdate rawd
                 , liftEff (clearRef st.trigger) *> pure Nothing
                 , repester st rawd
                 ]

main :: Eff (CoreEffects AppEffects) Unit
main = do
  initRef <- newRef Nothing
  app <- start { initialState: init initRef
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
