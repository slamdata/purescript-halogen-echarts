module Halogen.ECharts
  ( echarts
  , echarts'
  , EChartsState
  , EChartsQuery(..)
  , EChartsMessage(..)
  , EChartsEffects
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)

import Data.Foldable (for_, traverse_)
import Data.Foreign (Foreign)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))

import DOM (DOM)

import CSS.Geometry (width, height)
import CSS.Size (px)

import ECharts.Chart as EC
import ECharts.Theme as ETheme
import ECharts.Types as ET
import ECharts.Monad as EM
import ECharts.Types.Phantom as ETP

import Halogen as H
import Halogen.HTML.CSS (style)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


type EChartsState =
  { chart ∷ Maybe ET.Chart
  , width ∷ Int
  , height ∷ Int
  }

data EChartsQuery a
  = Init (Maybe ETheme.Theme) a
  | Dispose a
  | Set (EM.DSL ETP.OptionI) a
  | Reset (EM.DSL ETP.OptionI) a
  | Resize a
  | Clear a
  | SetDimensions { width ∷ Maybe Int, height ∷ Maybe Int } a
  | GetOptions (Maybe Foreign → a)
  | GetWidth (Int → a)
  | GetHeight (Int → a)

data EChartsMessage = Initialized

type EChartsEffects eff =
  ( echarts ∷ ET.ECHARTS
  , dom ∷ DOM
  , avar ∷ AVAR
  , err ∷ EXCEPTION
  , ref ∷ REF
  | eff
  )

type Dimensions = { width ∷ Int, height ∷ Int }

type HTML = H.ComponentHTML EChartsQuery

type DSL g = H.ComponentDSL EChartsState EChartsQuery EChartsMessage g

echarts
  ∷ ∀ eff g
  . Maybe ETheme.Theme
  → (MonadAff (EChartsEffects eff) g)
  ⇒ H.Component HH.HTML EChartsQuery (Dimensions /\ Unit) EChartsMessage g
echarts theme =
  echarts' theme \(dim /\ _) →
    Just $ H.action $ SetDimensions { width: Just dim.width, height: Just dim.height }

echarts'
  ∷ ∀ eff g i
  . (MonadAff (EChartsEffects eff) g)
  ⇒ Maybe ETheme.Theme
  → (Dimensions /\ i → Maybe (EChartsQuery Unit))
  → H.Component HH.HTML EChartsQuery (Dimensions /\ i)  EChartsMessage g
echarts' theme receiver = H.lifecycleComponent
  { initialState: \({width, height} /\ _) → { width, height, chart: Nothing }
  , render
  , eval
  , initializer: Just (H.action $ Init theme)
  , finalizer: Nothing
  , receiver
  }

render
  ∷ EChartsState → HTML
render state =
  HH.div
    [ HP.ref $ H.RefLabel "echarts"
    , style do
        height $ px $ toNumber state.height
        width $ px $ toNumber state.width
    ]
    []

eval
  ∷ ∀ eff g
  . (MonadAff (EChartsEffects eff) g)
  ⇒ EChartsQuery ~> (DSL g)
eval (Init theme next) = do
  H.getHTMLElementRef (H.RefLabel "echarts")
    >>= traverse_ \el → do
      chart ← liftEff $ maybe EC.init EC.initWithTheme theme el
      H.modify _{ chart = Just chart }
      H.raise Initialized
  pure next
eval (Dispose next) = do
  state ← H.get
  for_ state.chart $ liftEff <<< EC.dispose
  pure next
eval (Set opts next) = do
  state ← H.get
  for_ state.chart $ liftEff <<< EC.setOption opts
  pure next
eval (Reset opts next) = do
  state ← H.get
  for_ state.chart $ liftEff <<< EC.resetOption opts
  pure next
eval (Resize next) = do
  state ← H.get
  for_ state.chart $ liftEff <<< EC.resize
  pure next
eval (Clear next) = do
  state ← H.get
  for_ state.chart $ liftEff <<< EC.clear
  pure next
eval (SetDimensions { width, height } next) = do
  for_ width \w → do
    H.modify _{ width = w }
  for_ height \h → do
    H.modify _{ height = h }
  state ← H.get
  for_ state.chart $ liftEff <<< EC.resize
  pure next
eval (GetOptions continue) = do
  state ← H.get
  mbOptions ← for state.chart $ liftEff <<< EC.getOption
  pure $ continue mbOptions
eval (GetWidth continue) = do
  map continue $ H.gets _.width
eval (GetHeight continue) = do
  map continue $ H.gets _.height
