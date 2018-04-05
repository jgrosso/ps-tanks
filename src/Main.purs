module Main
       ( main
       ) where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)

import Pux (CoreEffects, EffModel, start)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Text.Smolder.HTML (div)
import Text.Smolder.Markup (text)

type State = Unit

data Event = Unit

foldp :: forall e. Event -> State -> EffModel State Event e
foldp _ _ = { state: unit, effects: [] }

view :: State -> HTML Event
view _ =
  div $ text "Hello, world!"

main :: forall e. Eff (CoreEffects e) Unit
main = do
  app <- start
    { initialState: unit
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
