module Halogen.ECharts
  ( echarts
  , EChartsState
  , EChartsQuery(..)
  , initialEChartsState
  , EChartsEffects
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable)

import Data.Foldable (for_)
import Data.Foreign (Foreign)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement)

import CSS.Geometry (width, height)
import CSS.Size (px)

import ECharts.Chart as EC
import ECharts.Types as ET
import ECharts.Monad as EM
import ECharts.Types.Phantom as ETP

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type EChartsState =
  { element ∷ Maybe HTMLElement
  , chart ∷ Maybe ET.Chart
  , width ∷ Int
  , height ∷ Int
  }

initialEChartsState ∷ Int → Int → EChartsState
initialEChartsState w h =
  { element: Nothing
  , chart: Nothing
  , width: w
  , height: h
  }

data EChartsQuery a
  = SetElement (Maybe HTMLElement) a
  | Init a
  | Dispose a
  | Set (EM.DSL ETP.OptionI) a
  | Reset (EM.DSL ETP.OptionI) a
  | Resize a
  | Clear a
  | SetHeight Int a
  | SetWidth Int a
  | GetOptions (Maybe Foreign → a)
  | GetWidth (Int → a)
  | GetHeight (Int → a)

type EChartsEffects eff =
  ( echarts ∷ ET.ECHARTS
  , dom ∷ DOM
  , avar ∷ AVAR
  , err ∷ EXCEPTION
  | eff
  )

type HTML = H.ComponentHTML EChartsQuery
type DSL g = H.ComponentDSL EChartsState EChartsQuery g

echarts
  ∷ ∀ eff g
  . (Affable (EChartsEffects eff) g)
  ⇒ H.Component EChartsState EChartsQuery g
echarts = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

render ∷ EChartsState → HTML
render state =
  HH.div
    [ HP.ref (H.action <<< SetElement)
    , style do
        height $ px $ toNumber state.height
        width $ px $ toNumber state.width
    ]
    []

eval
  ∷ ∀ eff g
  . (Affable (EChartsEffects eff) g)
  ⇒ EChartsQuery ~> (DSL g)
eval (SetElement el next) = do
  H.modify (_ { element = el })
  pure next
eval (Init next) = do
  state ← H.get
  for_ state.element \el → do
    chart ← H.fromEff (EC.init el ∷ Eff (EChartsEffects eff) ET.Chart)
    H.modify (_{chart = pure chart})
  pure next
eval (Dispose next) = do
  state ← H.get
  for_ state.chart \chart → do
    H.fromEff (EC.dispose chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (Set opts next) = do
  state ← H.get
  for_ state.chart \chart → do
    H.fromEff (EC.setOption opts chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (Reset opts next) = do
  state ← H.get
  for_ state.chart \chart → do
    H.fromEff (EC.resetOption opts chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (Resize next) = do
  state ← H.get
  for_ state.chart \chart →
    H.fromEff (EC.resize chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (Clear next) = do
  state ← H.get
  for_ state.chart \chart →
    H.fromEff (EC.clear chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (SetHeight h next) = do
  H.modify (_{height = h})
  state ← H.get
  for_ state.chart \chart →
    H.fromEff (EC.resize chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (SetWidth w next) = do
  H.modify (_{width = w})
  state ← H.get
  for_ state.chart \chart →
    H.fromEff (EC.resize chart ∷ Eff (EChartsEffects eff) Unit)
  pure next
eval (GetOptions continue) = do
  state ← H.get
  mbOptions ← for state.chart \chart →
    H.fromEff (EC.getOption chart ∷ Eff (EChartsEffects eff) Foreign)
  pure $ continue mbOptions
eval (GetWidth continue) = do
  map continue $ H.gets _.width
eval (GetHeight continue) = do
  map continue $ H.gets _.height
