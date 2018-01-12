module Model where

import Prelude
import Data.Maybe

import Data.Array (uncons)
import Data.Array as A
import Partial.Unsafe (unsafePartial)
import Optic.Core hiding (div, view)
import Optic.Lens
import Comms

type Response = Maybe Answer

newtype Answer
  = Answer { query :: String , results :: Array Detail }

newtype Detail
  = Detail { mx :: MxInfo, info :: Array String, expand :: Boolean }

newtype MxInfo
  = MxInfo { prior :: Int, hostname :: String }

derive instance eqMxInfo :: Eq MxInfo

_Answer :: Lens' Answer { query :: String, results :: Array Detail }
_Answer = lens (\(Answer x) -> x) (const Answer)

_Detail :: Lens' Detail { mx :: MxInfo, info :: Array String, expand :: Boolean } 
_Detail = lens (\(Detail x) -> x) (const Detail)

_MxInfo :: Lens' MxInfo { prior :: Int, hostname :: String }
_MxInfo = lens (\(MxInfo x) -> x) (const MxInfo)

query :: Lens' { query :: String, results :: Array Detail } String
query = lens _.query (_ { query = _ })

results :: Lens' { query :: String, results :: Array Detail } (Array Detail)
results = lens _.results (_ { results = _ })

mx :: Lens' { mx :: MxInfo, info :: Array String, expand :: Boolean } MxInfo
mx = lens _.mx (_ { mx = _ })

prior :: Lens' { prior :: Int, hostname :: String } Int
prior = lens _.prior (_ { prior = _ })

hostname :: Lens' { prior :: Int, hostname :: String } String
hostname = lens _.hostname (_ { hostname = _ })

info :: Lens' { mx :: MxInfo, info :: Array String, expand :: Boolean } (Array String)
info = lens _.info (_ { info = _ })

expand :: Lens' { mx :: MxInfo, info :: Array String, expand :: Boolean } Boolean
expand = lens _.expand (_ { expand = _ })

noAnswer :: Response
noAnswer = Nothing

annotate :: RawAnswer -> Answer
annotate (RawAnswer { query, results }) = Answer { query: query, results: tag <$> results }
  where
    tag :: RawMxInfo -> Detail
    tag (RawMxInfo { prior, hostname }) = Detail { mx: MxInfo { prior, hostname }, info: [], expand: false }

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

getHN :: Detail -> String
getHN x = x ^. _Detail..mx.._MxInfo..hostname

matchMX :: RawDetail -> (Detail -> Boolean)
matchMX = ((>>>) getHN) <<< ((==) <<< getmx)
