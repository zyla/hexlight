module Main where

import Prelude hiding (div)

import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (for_, sequence_, traverse_)
import Data.Maybe (Maybe(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequenceDefault, traverseDefault)
import Mock (demoText)
import Prelude (join)
import Pux (EffModel, CoreEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, h2, textarea)
import Text.Smolder.Markup (text, (#!))

data Event = InputChange DOMEvent

type Color = String
type State =
  { input :: String
  , colors :: Array Color
  }

matchInput :: String -> Maybe (Array (Maybe Color))
-- matchInput input = case regex "#[[:xdigit:]]+" global of
matchInput input = case regex "#[0-9a-fA-F]+" global of
  Right rx -> match rx input
  Left _ -> Nothing

-- r :: Array (Maybe Color) -> Maybe (HTML Event)
-- r c = traverse_ text <$> sequenceDefault c

renderColors :: Maybe (Array (Maybe Color)) -> HTML Event
renderColors Nothing = text "no match"
renderColors (Just colorMatches) =
  case sequenceDefault colorMatches of
    Nothing      -> text "no match"
    Just matches -> for_ matches \color -> do
      text color

init :: State
init = { input: "", colors: [] }

foldp :: Event -> State -> EffModel State Event (console :: CONSOLE, dom :: DOM)
foldp (InputChange ev) s=
  { state: s { input = targetValue ev }
  , effects: [ Aff.log "update state" *> pure Nothing ]
  }

view :: State -> HTML Event
view state = div do
  h2 $ text "paste text and code here"
  textarea #! onChange InputChange $ text demoText
  h2 $ text "colors found:"
  renderColors $ matchInput state.input

main ::  Eff (CoreEffects (console :: CONSOLE, dom :: DOM)) Unit
main = do
  log "starting app"
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  log "rendering"
  renderToDOM "#app" app.markup app.input
