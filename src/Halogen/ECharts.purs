module Halogen.ECharts
  ( echarts
  , EChartsState()
  , EChartsQuery(..)
  , initialEChartsState
  , EChartsEffects()
  ) where

import Prelude

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Aff.Free (class Affable)

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (Natural())

import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

import CSS.Geometry (width, height)
import CSS.Size (px)

import ECharts.Chart as EC
import ECharts.Effects as EE
import ECharts.Options as EO

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type EChartsState =
  { element :: Maybe HTMLElement
  , option :: Maybe EO.Option
  , chart :: Maybe EC.EChart
  , width :: Int
  , height :: Int
  }

initialEChartsState :: Int -> Int -> EChartsState
initialEChartsState w h =
  { element: Nothing
  , option: Nothing
  , chart: Nothing
  , width: w
  , height: h
  }

data EChartsQuery a
  = SetElement (Maybe HTMLElement) a
  | Init a
  | Dispose a
  | Set EO.Option a
  | Resize a
  | Refresh a
  | Clear a
  | SetHeight Int a
  | SetWidth Int a
  | GetOptions (Maybe EO.Option -> a)
  | GetWidth (Int -> a)
  | GetHeight (Int -> a)

type EChartsEffects eff =
  ( echartInit :: EE.ECHARTS_INIT
  , echartSetOption :: EE.ECHARTS_OPTION_SET
  , echartDispose :: EE.ECHARTS_DISPOSE
  , echartResize :: EE.ECHARTS_RESIZE
  , echartRefresh :: EE.ECHARTS_REFRESH
  , echartClear :: EE.ECHARTS_CLEAR
  , dom :: DOM
  , avar :: AVAR
  | eff
  )

type HTML = H.ComponentHTML EChartsQuery
type DSL g = H.ComponentDSL EChartsState EChartsQuery g

echarts
  :: forall eff g
   . (Affable (EChartsEffects eff) g)
  => H.Component EChartsState EChartsQuery g
echarts = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render :: EChartsState -> HTML
render state =
  HH.div
    [ HP.ref (H.action <<< SetElement)
    , style do
        height $ px $ toNumber state.height
        width $ px $ toNumber state.width
    ]
    []

eval
  :: forall eff g
   . (Affable (EChartsEffects eff) g)
  => Natural EChartsQuery (DSL g)
eval (SetElement el next) = do
  H.modify (_ { element = el })
  pure next
eval (Init next) = do
  state <- H.get
  case state.element of
    Nothing -> pure unit
    Just el -> do
      chart <- H.fromEff $ EC.init Nothing el
      H.modify (_{chart = pure chart})
  pure next
eval (Dispose next) = do
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.dispose chart
  pure next
eval (Set opts next) = do
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> do
      chart' <- H.fromEff $ EO.setOption opts true chart
      H.modify (_{ chart = pure chart', option = pure opts })
  pure next
eval (Resize next) = do
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.resize chart
  pure next
eval (Refresh next) = do
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.refresh chart
  pure next
eval (Clear next) = do
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.clear chart
  pure next
eval (SetHeight h next) = do
  H.modify (_{height = h})
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.resize chart
  pure next
eval (SetWidth w next) = do
  H.modify (_{width = w})
  state <- H.get
  case state.chart of
    Nothing -> pure unit
    Just chart -> H.fromEff $ EC.resize chart
  pure next
eval (GetOptions continue) = do
  map continue $ H.gets _.option
eval (GetWidth continue) = do
  map continue $ H.gets _.width
eval (GetHeight continue) = do
  map continue $ H.gets _.height
