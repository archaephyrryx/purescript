module Model where

import Prelude
import Data.Maybe

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
