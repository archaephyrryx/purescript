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
import Data.Generic.Rep
import Data.List as L
import Data.List.NonEmpty (toList, NonEmptyList)
import Data.Maybe
import Data.Tuple

import Optic.Core hiding (div, view)
import Optic.Lens

import Data.Argonaut

type RawResponse = RawAnswer
newtype RawAnswer = RawAnswer { query :: String, results :: Array RawDetail } -- (Tuple Int String)  }
newtype RawDetail = RawDetail { mx :: String, info :: Array String }

derive instance genericRawDetail :: Generic RawDetail _
derive instance genericRawAnswer :: Generic RawAnswer _

opts = defaultOptions { unwrapSingleConstructors = true }

instance decodeRawAnswer :: Decode RawAnswer where
  decode = genericDecode opts
instance decodeRawDetail :: Decode RawDetail where
  decode = genericDecode opts

one :: forall a. a -> List a
one x = x:Nil

parseJSON r = runExcept (genericDecodeJSON opts r.response) :: Either (NonEmptyList ForeignError) RawResponse

orError :: forall a b. (a -> b) -> Either (NonEmptyList ForeignError) a -> Either (List String) b
orError f = bimap (toList <<< map renderForeignError) f

similar :: forall a b. Eq b => (a -> b) -> a -> a -> Boolean
similar f x y = f x == f y
