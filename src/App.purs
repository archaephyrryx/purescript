module App where

import Prelude hiding (div, one)

import Data.Maybe (Maybe)
import Control.Monad.Aff (Fiber)
import Control.Monad.Eff.Ref (REF)
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import Pux (CoreEffects)
import Event

type AppEffects = (ajax :: AJAX, ref :: REF, dom :: DOM)

type Trigger = Fiber (CoreEffects AppEffects) (Maybe Event)
