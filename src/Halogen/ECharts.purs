-- | This module defines an adapter between Halogen's widget API and
-- | the `purescript-echarts` library.

module Halogen.ECharts 
  ( EChartsContext()
  , ECEffects()
  , chart
  , newContext
  , init
  , postRender
  ) where

import DOM

import Data.Int
import Data.Maybe
import Data.Function
import Data.Foldable (for_)

import qualified Data.StrMap as M

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Data.DOM.Simple.Window
import Data.DOM.Simple.Document

import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import qualified ECharts.Chart as EC
import qualified ECharts.Options as EC
import qualified ECharts.Effects as EC

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

type ECEffects eff = ( echartInit :: EC.EChartInit
                     , echartSetOption :: EC.EChartOptionSet
                     , echartDispose :: EC.EChartDispose
                     , dom :: DOM 
                     , ref :: Ref
                     | eff)

-- | The ECharts context, which should be passed to the initialization 
-- | and post-render hooks.
newtype EChartsContext = EChartsContext (RefVal (M.StrMap EC.EChart))

-- | Create a new ECharts context.
newContext :: forall eff. Eff (ref :: Ref | eff) EChartsContext
newContext = EChartsContext <$> newRef M.empty

-- | The initialization hook, which creates ECharts components.
init :: forall eff. EChartsContext -> HTMLElement -> Eff (ECEffects eff) Unit
init (EChartsContext ref) node = do
  els <- querySelector "[data-halogen-echarts-id]" node
  for_ els \el -> do
    -- Get the key
    key <- getAttribute "data-halogen-echarts-id" el
    -- Setup the ECharts component
    m <- readRef ref
    ec <- case M.lookup key m of
      Nothing -> do
        -- Create a new ECharts object and store it in the map
        ec <- EC.init Nothing el
        modifyRef ref (M.insert key ec)
        return ec
      Just ec -> return ec
    updateOptions el ec
  
-- | The post-render hook, which updates any ECharts components.
postRender :: forall eff driver. EChartsContext -> HTMLElement -> driver -> Eff (ECEffects eff) Unit
postRender (EChartsContext ref) node _ = do
  els <- querySelector "[data-halogen-echarts-id]" node
  m <- readRef ref
  for_ els \el -> do
    -- Get the key
    key <- getAttribute "data-halogen-echarts-id" el
    for_ (M.lookup key m) (updateOptions el)
      
foreign import getOptions
  "function getOptions(node) {\
  \  return function() {\
  \    return node['halogen-echarts-options'];\
  \  };\
  \}" :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) EC.Option
      
updateOptions :: forall eff. HTMLElement -> EC.EChart -> Eff (ECEffects eff) Unit
updateOptions node ec = void do
  opts <- getOptions node
  EC.setOption opts false ec

-- | Create a chart component
chart :: forall i. String -> Number -> EC.Option -> H.HTML i
chart key height opts = H.div [ A.style (A.styles (M.singleton "height" (show height <> "px")))
                              , dataHalogenEChartsID key
                              , dataHalogenEChartsOptions opts
                              ] []
  where
  -- | Custom attributes
  dataHalogenEChartsID :: forall i. String -> A.Attr i
  dataHalogenEChartsID = A.attr $ A.attributeName "data-halogen-echarts-id"
  
  dataHalogenEChartsOptions :: forall i. EC.Option -> A.Attr i
  dataHalogenEChartsOptions = A.attr $ A.attributeName "halogen-echarts-options"
  
instance optionIsAttribute :: A.IsAttribute EC.Option where
  toAttrString _ _ = ""