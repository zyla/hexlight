module Main where

import Prelude hiding (div)

import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequenceDefault)
import Mock (demoText)
import Pux (EffModel, CoreEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, h2, textarea, span)
import Text.Smolder.HTML.Attributes (className, style) as Attr
import Text.Smolder.Markup (text, (!), (#!))

data Event = InputChange DOMEvent

type Color = String
type State =
  { input :: String
  , colors :: Array Color
  }

matchInput :: String -> Maybe (Array (Maybe Color))
-- TODO: match only up to 6 hex values in regex
matchInput input = case regex "#[0-9a-fA-F]+" global of
  Right rx -> match rx input
  Left _ -> Nothing

-- r :: Array (Maybe Color) -> Maybe (HTML Event)
-- r c = traverse_ text <$> sequenceDefault c

-- TODO: fmap or bind the 'no match' one up
-- TODO: provide default value for 'Nothing' case (removing all HTML here)
showColors :: Maybe (Array (Maybe Color)) -> (Color -> HTML Event) -> HTML Event
showColors Nothing show = show "no match"
showColors (Just colorMatches) show =
  case sequenceDefault colorMatches of
    Nothing      -> show "no match"
    Just matches -> for_ matches show

init :: State
init = { input: "", colors: [] }

foldp :: Event -> State -> EffModel State Event (console :: CONSOLE, dom :: DOM)
foldp (InputChange ev) s =
  { state: s { input = targetValue ev }
  , effects: []
  }

view :: State -> HTML Event
view state = div ! Attr.className "layout" $ do
  -- TODO CSS: make it all less ugly
  h2 $ text "paste text and code here"
  div do
    textarea #! onChange InputChange ! Attr.className "pure-input-1" $ text demoText
  h2 $ text "colors found:"
  -- TODO: fg color white or black depending on bg
  --   sa https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color#3943023
  let showColor c = div ! Attr.className "pure-u-1-8" $ do
                      span ! Attr.style ("background-color: " <> c) $ text c
  -- TODO CSS: make grid flow
  div ! Attr.className "pure-g" $ do
    showColors (matchInput state.input) showColor

main ::  Eff (CoreEffects (console :: CONSOLE, dom :: DOM)) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
