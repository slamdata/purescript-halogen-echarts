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


import ECharts.Chart
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item

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
  simpleData = Value <<< Simple

  opts :: Option
  opts = Option $ optionDefault
                  { xAxis = Just $ OneAxis $ Axis $ axisDefault 
                       { "type" = Just CategoryAxis
                       , boundaryGap = Just $ CatBoundaryGap false
                       , "data" = Just $ CommonAxisData <$> [ "Monday", "Tuesday", "Wednesday"
                                                            , "Thursday", "Friday", "Saturday", "Sunday"
                                                            ]
                       }
                  , yAxis = Just $ OneAxis $ Axis $ axisDefault
                       { "type" = Just ValueAxis
                       }
                  , series = Just $ Just <$> 
                      [ LineSeries 
                          { common: universalSeriesDefault 
                              { name = Just "email marketing"
                              }
                          , lineSeries: lineSeriesDefault
                             { stack = Just "total"
                             , "data" = Just $ simpleData <$> [120, 132, 101, 134, 90, 230, 210]
                             }
                          }
                      ]
                  }

main = do
  Tuple node _ <- runUI ui
  appendToBody node
