module Main where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Traversable (for_)
import Pux (EffModel, noEffects, start)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onPaste)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (div, h2, textarea)
import Text.Smolder.Markup ((#!), text)

data Event = Hello | World

type State = { colors :: Array String  }
init :: State
init = { colors: ["#f00f00", "#001337"] }

foldp :: ∀ fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp _ state = noEffects $ state

view :: State -> HTML Event
view state = div do
  h2 $ text "paste text and code here"
  textarea #! onPaste (const Hello) $ text ""
  h2 $ text "colors found"
  for_ state.colors \c -> do
    text c

main :: forall fx. Eff (channel :: CHANNEL, exception :: EXCEPTION, dom :: DOM | fx) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
