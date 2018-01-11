module Main where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Traversable (for_)
import Mock (demoText)
import Pux (EffModel, noEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onPaste, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (button, div, h2, textarea)
import Text.Smolder.Markup ((#!), text)

data Event = InputChange DOMEvent

type Color = String
type State =
  { input :: String
  , colors :: Array Color
  }

parseInput :: String -> Array Color
parseInput i = []

init :: State
init = { input: "", colors: [] }

foldp :: ∀ fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (InputChange event) state = noEffects $ state { input = targetValue event }

view :: State -> HTML Event
view state = div do
  h2 $ text "paste text and code here"
  textarea $ text demoText #! onPaste InputChange #! onChange InputChange
  -- button #! onClick (const Hello) $ text "eval"
  h2 $ text "colors found:"
  for_ state.colors \c -> do
    text c
  h2 $ text "input:"
  text state.input

main :: ∀ fx. Eff (channel :: CHANNEL, exception :: EXCEPTION, dom :: DOM | fx) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
