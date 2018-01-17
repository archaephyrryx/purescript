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

-- | Kill any fiber currently stored in the trigger-latch, without modifying the latch's contents
cancel :: Ref (Maybe Process) -> Aff (CoreEffects AppEffects) Unit
cancel proc =
  liftEff (readRef proc) >>= maybe (pure unit) (killFiber (error "late"))

-- | Reset the contents of the trigger-latch, discarding any reference to a fiber that might be stored in it (alive or dead)
clearRef :: forall x fx. Ref (Maybe x) -> Eff (ref :: REF | fx) Unit
clearRef ref = writeRef ref Nothing

-- | Perform a string-argumented query operation in the background, storing a reference in the trigger latch, firing the event it returns if it completed execution and doing nothing if it was terminated
resolve :: (String -> Aff (CoreEffects AppEffects) (Maybe Event))
        -> String
        -> Ref (Maybe Process)
        -> Aff (CoreEffects AppEffects) (Maybe Event)
resolve f q proc = do
  fib <- forkAff $ f q
  liftEff $ writeRef proc (Just fib)
  res <- try (joinFiber fib)
  pure $ either (const Nothing) id res

-- | String-argumented query operation for retrieving details specific to the indicated domain name
getHost :: String
        -> Aff (CoreEffects AppEffects) (Maybe Event)
getHost q = do
  res <- attempt $ get (serverUrl <> q)
  let copy = either (Left <<< one <<< show) (orError (Just <<< annotate) <<< parseJSON) res
  pure $ Just $ Inbound $ ReDomain copy

-- | Generator for a string-argumented query operation that takes a domain-name to report to the server, and returns a function on an MX hostname argument to be provided
getMX :: String
      -> String
      -> Aff (CoreEffects AppEffects) (Maybe Event)
getMX dom q = do
  res <- attempt $ get (serverUrl <> dom <> "/" <> q)
  let copy = either (Left <<< one <<< show) (orError id <<< parseJSON) res
  pure $ Just $ Inbound $ ReMxHost copy

--| State update indicating that a particular MX host's details are currently in the process of being retrieved, implemented temporarily as a no-op
listen :: State -> String -> State
listen = const

--| In reponse to the receival of an MX host's information, remove it from the query-queue and trigger a query for the next unresolved host, if any remain
repester :: State
         -> RawDetail
         -> Aff (CoreEffects AppEffects) (Maybe Event)
repester stat rawd =
  let rem = complete rawd stat.queue
   in pure $ (Outbound <<< MXHostQuery <<< _.head) <$> L.uncons rem

--| Non-event-producing effect for terminating the extant XMLHTTP request fiber
kill :: State -> Aff (CoreEffects AppEffects) (Maybe Event)
kill st = cancel st.process *> pure Nothing

--| Non-event-producing effect for clearing the request fiber latch
dropRef :: State -> Aff (CoreEffects AppEffects) (Maybe Event)
dropRef st = liftEff (clearRef st.process) *> pure Nothing

-- | Accumulator function for folding event-timelines into state updates and reactions
foldp :: Event -> State -> EffModel State Event (AppEffects)
foldp (UI x) st = case x of
    Submit -> onlyEffects st [ pure $ Just $ Pending $ DomainWait st.inp
                             , pure $ Just $ Outbound $ DomainQuery st.inp
                             ]
    Cancel -> onlyEffects st [ kill st *> dropRef st *> (pure $ Just $ Resolve Cancelled) ]
    InputChange ev -> noEffects $ st { inp = targetValue ev }
    Toggle det     -> noEffects $ st { resp = toggle det <$> st.resp }
foldp (Outbound x) st = case x of
    DomainQuery str -> onlyEffects st [ kill st *> dropRef st *> resolve getHost str st.process ]
    MXHostQuery str -> onlyEffects st [ pure $ Just $ Pending $ MXHostWait str
                                      , dropRef st *> resolve (getMX $ domainName st) str st.process
                                      ]
foldp (Inbound x) st = case x of
    ReDomain (Left strs)  -> onlyEffects st [ pure $ Just $ Resolve $ Error strs ]
    ReDomain (Right resp) -> onlyEffects st [ dropRef st *> (pure $ Just $ Resolve $ Update resp) ]
    ReMxHost (Left strs)  -> onlyEffects st [ pure $ Just $ Resolve $ Error strs ]
    ReMxHost (Right rawd) -> onlyEffects st [ pure $ Just $ Resolve $ MXUpdate rawd
                                            , dropRef st *> repester st rawd
                                            ]
foldp (Pending x) st = case x of
    DomainWait str     -> noEffects $ st { waiting = true }
    MXHostWait str -> noEffects $ listen st str
foldp (Resolve x) st = case x of
    Cancelled -> noEffects $ st { waiting = false }
    Error strs -> noEffects $ st { err = strs<>st.err, waiting = false }
    MXUpdate rawd -> noEffects $ st { resp = expound rawd <$> st.resp, queue = complete rawd st.queue }
    Update resp -> let st' = update resp st
                    in { state: st' , effects: [ pure $ (Outbound <<< MXHostQuery <<< _.head) <$> L.uncons st'.queue ] }

main :: Eff (CoreEffects AppEffects) Unit
main = do
  initRef <- newRef Nothing
  app <- start { initialState: init initRef
               , view
               , foldp
               , inputs: []
               }
  renderToDOM "#app" app.markup app.input
