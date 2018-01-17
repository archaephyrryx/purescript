module App where

import Prelude hiding (div, one)

import Data.Maybe (Maybe)
import Control.Monad.Aff (Fiber)
import Control.Monad.Eff.Ref (REF)
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import Pux (CoreEffects)
import Event


--| Type alias for the set of all side-effects of the application's components
type AppEffects = (ajax :: AJAX, ref :: REF, dom :: DOM)

--| Type alias for fibers representing forked Aff monad calls
type Process = Fiber (CoreEffects AppEffects) (Maybe Event)
