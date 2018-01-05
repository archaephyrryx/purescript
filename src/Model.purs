module Model where

import Prelude
import Data.Maybe

import Optic.Core hiding (div, view)
import Optic.Lens
import Comms

type Response = Maybe Answer

newtype Answer = Answer { query :: String, results :: Array Detail }

newtype Detail = Detail { mx :: String, info :: Array String, expand :: Boolean }

_Answer :: Lens' Answer { query :: String, results :: Array Detail }
_Answer = lens (\(Answer x) -> x) (const Answer)

_Detail :: Lens' Detail { mx :: String, info :: Array String, expand :: Boolean } 
_Detail = lens (\(Detail x) -> x) (const Detail)

query :: Lens' { query :: String, results :: Array Detail } String
query = lens _.query (_ { query = _ })

results :: Lens' { query :: String, results :: Array Detail } (Array Detail)
results = lens _.results (_ { results = _ })

mx :: Lens' { mx :: String, info :: Array String, expand :: Boolean } String
mx = lens _.mx (_ { mx = _ })

info :: Lens' { mx :: String, info :: Array String, expand :: Boolean } (Array String)
info = lens _.info (_ { info = _ })

expand :: Lens' { mx :: String, info :: Array String, expand :: Boolean } Boolean
expand = lens _.expand (_ { expand = _ })

noAnswer :: Response
noAnswer = Nothing

annotate :: RawAnswer -> Answer
annotate (RawAnswer { query, results }) = Answer { query: query, results: tag <$> results }
  where
    tag :: RawDetail -> Detail
    tag (RawDetail { mx, info }) = Detail { mx: mx, info: info, expand: false }
