module Main where

import Prelude

import CSS (CSS, backgroundColor, color, display, em, height, inlineBlock, margin, marginLeft, pct, px, width)
import CSS as CSS
import CSS.Color (black, fromHexString, luminance, white)
import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (getTextInputValue)
import Specular.FRP (Dynamic, dynamic_, for, holdDyn)

matchInput :: String -> Maybe (Array (Maybe String))
matchInput input = case regex "#[0-9a-fA-F]{6}" global of
    Right rx -> match rx input
    Left _ -> Nothing

showColors :: forall m. MonadWidget m => Array String -> m Unit
showColors colors =
    elAttr "div" (style (width (80.0 # pct)))
        $ for_ colors \c -> do
            case fromHexString c of
                Nothing -> text "error"
                Just col -> do
                    elAttr "span" (style do
                                    display inlineBlock
                                    margin  (4.0 # px) (4.0 # px) (4.0 # px) (4.0 # px)
                                    textAlign center
                                    width (7.0 # em)
                                    backgroundColor col
                                    color (if luminance col > 0.179 then black else white) )
                         $ text c

initialInput :: String
initialInput = ""

app :: forall m. MonadWidget m => m Unit
app =
    elAttr "div" (style (marginLeft (5.0 # pct)))
        $ do
        el "h2" $ text "paste text and code here"
        el "div" do
            inputD <- textareaOnInput
              initialInput
              (style do width (80.0 # pct)
                        height (6.0 # em))

            dynamic_ $ for inputD $ \input ->
              case (traverse join $ sequence (matchInput input)) of
                  Nothing -> el "div" $ el "span" $ text "no colors found"
                  Just colors -> do
                      showColors colors

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody app


-- utilities

style :: CSS -> Attrs
style css = "style" := renderCSSInline css

renderCSSInline :: CSS -> String
renderCSSInline = fromMaybe "" <<< CSS.renderedInline <<< CSS.render

-- | Place a `<textarea>` in the DOM. Returns Dynamic that changes on `input` event,
-- | and contains the current value of the textarea.
textareaOnInput :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textareaOnInput initial attrs = do
  Tuple node _ <- elDynAttr' "textarea" (pure attrs) (text initial)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "input" node
  holdDyn initial changed
