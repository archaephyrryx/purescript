module Event where

import Prelude hiding (div, one)

import Data.Either (Either)
import Data.List (List)
import Pux.DOM.Events (DOMEvent)
import Comms (RawDetail)
import Model (Response, Detail)

type Errorful a = Either (List String) a

data Event = UI Action 
           | Reflex Reaction
           | Inbound Traffic
           | Anticipation Status
           | Resolve Consequence

data Action = Submit
            | Cancel
            | InputChange DOMEvent
            | Toggle Detail

data Reaction = Clear
              | Pester String

data Status = Await String
            | Listening String

data Traffic = Remote (Errorful Response)
             | FollowUp (Errorful RawDetail)

data Consequence = Cancelled
                 | Cleared
                 | Error (List String)
                 | Update Response
                 | MXUpdate RawDetail
