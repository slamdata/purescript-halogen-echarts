module Main where

import Data.Void
import Data.Tuple
import Data.Maybe

import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import ECharts.Options
import ECharts.Tooltip
import ECharts.Legend
import ECharts.Coords

import Halogen
import Halogen.Signal
import Halogen.Component
import Halogen.ECharts

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.Internal.VirtualDOM as H

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- document globalWindow
  b <- body w 
  appendChild b e

ui :: forall m i eff. (Applicative m) => Component (H.Widget (ECEffects eff) i) m i i
ui = component (pure (H.placeholder (chart "example-chart" zero opts)))
  where  
  opts :: Option
  opts = Option $ optionDefault
                  { tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis}
                  , legend = Just $ Legend legendDefault
                                           { x = Just XLeft
                                           , "data" = Just $ legendItemDefault <$> [ "foo", "bar", "baz" ]
                                           }
                  }

main = do
  Tuple node _ <- runUI ui
  appendToBody node