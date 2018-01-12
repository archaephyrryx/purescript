module State where

import Prelude

import Control.Monad.Eff.Ref (newRef, Ref, REF, readRef, writeRef)
import Control.Applicative

import Data.Array as A
import Data.List (List(..))
import Data.List as L
import Data.Maybe
import Optic.Core
import Optic.Lens
import Pux (CoreEffects)

import Comms
import Model
import Event
import App

type State =
  { inp :: String
  , resp :: Response
  , queue :: List String
  , waiting :: Boolean
  , err :: List String
  , trigger :: Ref (Maybe Trigger)
  }

init :: (Ref (Maybe Trigger)) -> State
init trig =
  { inp: ""
  , resp: noAnswer
  , queue: Nil
  , waiting: false
  , err: Nil
  , trigger: trig
  }

domainName :: State -> String
domainName st = maybe (st.inp) (\x -> x ^. _Answer..query) st.resp

update :: Response -> State -> State
update resp st = st { resp = resp, waiting = false, err = Nil, queue = requeue resp }

mxupdate :: RawDetail -> State -> State
mxupdate rawd st = st { resp = (expound rawd <$> st.resp), waiting = false, err = Nil }

requeue :: Response -> List String
requeue Nothing = Nil
requeue (Just x) = A.toUnfoldable $ map getHN $ x ^. _Answer..results

complete :: RawDetail -> List String -> List String
complete rawd = L.filter (\x -> x /= getmx rawd)
