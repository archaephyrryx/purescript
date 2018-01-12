module UI where

import Prelude hiding (div)

import Data.Maybe
import Data.Traversable (traverse)
import Optic.Core hiding (div, view)
import Optic.Lens
import Pux.DOM.Events (onClick, onChange)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML hiding (map)
import Text.Smolder.HTML.Attributes hiding (span, form, id)
import Text.Smolder.Markup

import Model
import Event
import State

expandCollapse :: Detail -> HTML Event
expandCollapse det = do
    button #! onClick (const $ UI $ Toggle det) $ text (excolText $ det ^. _Detail..expand)
  where
    excolText true = "◀︎"
    excolText false = "▶︎"

viewDetail :: Detail -> HTML Event
viewDetail det =
  div $ do
    div $ do
      p $ do
        expandCollapse det
        span $ text $ getHN det
      if det ^. _Detail..expand
        then
          pre $ void $ traverse (\x -> span $ text x) $ det ^. _Detail..info
        else
          pure unit

viewResponse :: Response -> HTML Event
viewResponse Nothing = span $ text "Enter a domain to retrieve results for."
viewResponse (Just ans) = do
  span $ text $ ans ^. _Answer..query
  void $ traverse viewDetail $ ans ^. _Answer..results

view :: State -> HTML Event
view stat =
  div $ do
    div $ do
      input ! type' "text" ! value stat.inp #! onChange (UI <<< InputChange)
      button ! type' "submit" #! onClick (const $ UI Submit) $ text "Go"
      button #! onClick (const $ UI Cancel) $ text "Cancel"
    pre $ void $ traverse (\x -> span $ text x) stat.err
    if stat.waiting
      then do
        span $ text $ "Retrieving results for domain "<>stat.inp<>" (please wait...)"
        span $ text "(If nothing changes the page isn't necessarily frozen)"
      else
        div $ viewResponse stat.resp
