module Main where

import Data.Int
import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either

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

import qualified Halogen.ECharts as EC

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.Internal.VirtualDOM as H

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- document globalWindow
  b <- body w 
  appendChild b e

-- | Toggle the smooth rendering flag
data Input = ToggleSmooth

-- | The state object:
-- |
-- | - the version of the chart, to enable repainting
-- | - a flag to toggle smooth lines
type State = { version :: Int, smooth :: Boolean }

ui :: forall m eff. (Applicative m) => Component m Input Input
ui = render <$> stateful { version: zero :: Int, smooth: false } update
  where
  render :: State -> H.HTML (m Input)
  render o = H.div_ [ EC.chart "example-chart" 400 (opts o.smooth)
                    , H.p_ [ H.button [ A.onClick (A.input_ ToggleSmooth) ] [ H.text "Toggle Smooth" ] ]
                    ]
  
  -- | Update the state (note, we bump the version to ensure the chart gets updated)
  update :: State -> Input -> State
  update o _ = o { smooth = not o.smooth }
  
  -- | Line series data
  opts :: Boolean -> Option
  opts smooth = Option $ optionDefault
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
                             , "data" = Just $ Value <<< Simple <$> [120, 132, 101, 134, 90, 230, 210]
                             , smooth = Just smooth
                             }
                          }
                      ]
                  }

main = do
  ctx <- EC.newContext    
    
  Tuple node _ <- runUIWith ui (EC.postRender ctx)
  appendToBody node
  
  EC.init ctx node
