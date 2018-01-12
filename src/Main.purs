module Main where

import Prelude hiding (div, one)

import Control.Applicative
import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, Ref, REF, readRef, writeRef)

import Data.Array hiding (span, (:), cons, init, (..))
import Data.Either
import Data.Function
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax (get)
import Pux (CoreEffects, EffModel, start, noEffects, onlyEffects)
import Pux.DOM.Events (targetValue)
import Pux.Renderer.React (renderToDOM)

import App
import Comms
import Event
import Model
import State
import UI
import Url (serverUrl)

cancel :: Ref (Maybe Trigger) -> Aff (CoreEffects AppEffects) Unit
cancel trig =
  liftEff (readRef trig) >>= maybe (pure unit) (killFiber (error "late"))

clearRef :: forall x fx. Ref (Maybe x) -> Eff (ref :: REF | fx) Unit
clearRef ref = writeRef ref Nothing

resolve :: (String -> Aff (CoreEffects AppEffects) (Maybe Event))
        -> String
        -> Ref (Maybe Trigger)
        -> Aff (CoreEffects AppEffects) (Maybe Event)
resolve f q trig = do
  fib <- forkAff $ f q
  liftEff $ writeRef trig (Just fib)
  res <- try (joinFiber fib)
  pure $ either (const Nothing) id res

getHost :: String
        -> Aff (CoreEffects AppEffects) (Maybe Event)
getHost q = do
  res <- attempt $ get (serverUrl <> q)
  let copy = either (Left <<< one <<< show) (orError (Just <<< annotate) <<< parseJSON) res
  pure $ Just $ Inbound $ Remote copy

getMX :: String
      -> String
      -> Aff (CoreEffects AppEffects) (Maybe Event)
getMX dom q = do
  res <- attempt $ get (serverUrl <> dom <> "/" <> q)
  let copy = either (Left <<< one <<< show) (orError id <<< parseJSON) res
  pure $ Just $ Inbound $ FollowUp copy

{-
 - this function is used to update the state to indicate that the
 - results for a given mx host (the string argument) are being listened
 - for, but is currently a no-op because our model does not have any kind
 - of indicator for this state currently. XXX Make this do something at
 - some point
 -}
listen :: State -> String -> State
listen = const

repester :: State
         -> RawDetail
         -> Aff (CoreEffects AppEffects) (Maybe Event)
repester stat rawd =
  let rem = complete rawd stat.queue
   in pure $ (Reflex <<< Pester <<< _.head) <$> L.uncons rem

kill :: State -> Aff (CoreEffects AppEffects) (Maybe Event)
kill st = cancel st.trigger *> pure Nothing

dropRef :: State -> Aff (CoreEffects AppEffects) (Maybe Event)
dropRef st = liftEff (clearRef st.trigger) *> pure Nothing

foldp :: Event -> State -> EffModel State Event (AppEffects)
foldp (UI x) st = case x of
    Submit -> onlyEffects st [ pure $ Just $ Reflex Clear ]
    Cancel -> onlyEffects st [ kill st
                             , dropRef st
                             , pure $ Just $ Resolve Cancelled
                             ]
    InputChange ev -> noEffects $ st { inp = targetValue ev }
    Toggle det     -> noEffects $ st { resp = toggle det <$> st.resp }
foldp (Reflex x) st = case x of
    Clear -> onlyEffects st [ kill st
                            , dropRef st
                            , pure $ Just $ Resolve Cleared
                            ]

    Pester str -> onlyEffects st [ resolve (getMX $ domainName st) str st.trigger
                                 , pure $ Just $ Anticipation $ Listening str
                                 ]
foldp (Inbound x) st = case x of
    Remote (Left strs) -> onlyEffects st [ pure $ Just $ Resolve $ Error strs ]
    Remote (Right resp) -> onlyEffects st [ pure $ Just $ Resolve $ Update resp
                                          , dropRef st
                                          ]
    FollowUp (Left strs) -> onlyEffects st [ pure $ Just $ Resolve $ Error strs ]
    FollowUp (Right rawd) -> onlyEffects st [ pure $ Just $ Resolve $ MXUpdate rawd
                                            , dropRef st
                                            ,  repester st rawd
                                            ]
foldp (Anticipation x) st = case x of
    Await str     -> noEffects $ st { waiting = true }
    Listening str -> noEffects $ listen st str
foldp (Resolve x) st = case x of
    Cancelled -> noEffects $ st { waiting = false }
    Cleared  -> onlyEffects st [ resolve getHost st.inp st.trigger
                               , pure $ Just $ Anticipation $ Await st.inp
                               ]
    Error strs -> noEffects $ st { err = strs<>st.err, waiting = false }
    MXUpdate rawd -> noEffects $ st { resp = expound rawd <$> st.resp, queue = complete rawd st.queue }
    Update resp -> let st' = update resp st
                    in { state: st' , effects: [ pure $ (Reflex <<< Pester <<< _.head) <$> L.uncons st'.queue ] }

main :: Eff (CoreEffects AppEffects) Unit
main = do
  initRef <- newRef Nothing
  app <- start { initialState: init initRef
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
