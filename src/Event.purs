module Event where

import Prelude hiding (div, one)

import Data.Either (Either)
import Data.List (List)
import Pux.DOM.Events (DOMEvent)
import Comms (RawDetail)
import Model (Response, Detail)

-- | Type alias for results of operations that might produce a list of errors instead of a value of the desired type
type Errorful a = Either (List String) a

-- | Union type of sub-events, divided for convenience
data Event = UI Action
           | Outbound Query
           | Inbound Reply
           | Pending Status
           | Resolve StateUpdate

-- | ADT for all events that represent actions instigated by user interaction
data Action = Submit
            | Cancel
            | InputChange DOMEvent
            | Toggle Detail

-- | ADT for all events that are fired in direct response to conceptually unrelated events
data Query = DomainQuery String
           | MXHostQuery String

-- | ADT for all events that represent entry into a transitional state, after a trigger and before a resolution
data Status = DomainWait String
            | MXHostWait String

-- | ADT for all events that represent direct network traffic from the server in response to queries
data Reply = ReDomain (Errorful Response)
           | ReMxHost (Errorful RawDetail)

-- | ADT for all events that represent complete state transitions in response to `Action`, `Reaction`, and `Traffic` events
data StateUpdate = Cancelled
                 | Error (List String)
                 | Update Response
                 | MXUpdate RawDetail
