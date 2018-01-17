module Comms where

import Prelude

import Control.Applicative
import Control.Bind
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except

import Data.List
import Data.Bifunctor (bimap)
import Data.Either
import Data.Function
import Data.Foreign (ForeignError, renderForeignError)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode, genericDecodeJSON, genericEncodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode)
import Data.Generic.Rep
import Data.List as L
import Data.List.NonEmpty (toList, NonEmptyList)
import Data.Maybe
import Data.Tuple

import Optic.Core hiding (div, view)
import Optic.Lens

import Data.Argonaut

--| Analogy for `Response`-`Answer` relationship in Model
type RawResponse = RawAnswer

--| Record newtype for server-side information retrieved for each domain query
newtype RawAnswer
  = RawAnswer { query :: String, results :: Array RawMxInfo }

--| Record newtype used to enumerate the MX hosts of a domain, as part of the initial domain query
newtype RawMxInfo
  = RawMxInfo { prior :: Int, hostname :: String }

--| Record newtye for server-side information retrieved per mx host of a given domain
newtype RawDetail
  = RawDetail { mx :: String, info :: Array String }

--| Unwrapper-accessor for hostname of a `RawDetail`
getmx :: RawDetail -> String
getmx (RawDetail { mx, info }) = mx

derive instance genericRawDetail :: Generic RawDetail _
derive instance genericRawAnswer :: Generic RawAnswer _
derive instance genericRawMxInfo :: Generic RawMxInfo _

instance decodeRawAnswer :: Decode RawAnswer where
  decode = genericDecode opts
instance decodeRawDetail :: Decode RawDetail where
  decode = genericDecode opts
instance decodeRawMxInfo :: Decode RawMxInfo where
  decode = genericDecode opts

--| Trivial helper function for singleton listmaking
one :: forall a. a -> List a
one x = x:Nil

type ParseAttempt a = Either (NonEmptyList ForeignError) a

opts = defaultOptions { unwrapSingleConstructors = true }

-- | Attempts to decode a record with a string field `response` as a generically decodeable polymorphic type
parseJSON :: forall a b r. Generic a b => GenericDecode b => { response :: String | r } -> ParseAttempt a
parseJSON = runExcept <<< genericDecodeJSON opts <<< _.response

-- | Transforms left branch of a `runExcept` call into a more convenient format
orError :: forall a b. (a -> b) -> Either (NonEmptyList ForeignError) a -> Either (List String) b
orError f = bimap (toList <<< map renderForeignError) f

-- | Returns equality test of the images of two values under a specified mapping (as per Haskell's @compare `on` f@)
similar :: forall a b. Eq b => (a -> b) -> a -> a -> Boolean
similar f x y = f x == f y
