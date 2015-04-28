-- | This module defines an adapter between Halogen's widget API and
-- | the `purescript-echarts` library.

module Halogen.ECharts (ECEffects(), chart) where

import DOM

import Data.Int
import Data.Maybe
import Data.DOM.Simple.Types

import Control.Monad (when)
import Control.Monad.Eff

import qualified ECharts.Chart as EC
import qualified ECharts.Options as EC
import qualified ECharts.Effects as EC

import Halogen.HTML.Widget
import Halogen.Internal.VirtualDOM (Widget())

-- todo: PR on simple-dom
foreign import createDiv
  "function createDiv() {\
  \  return document.createElement('div');\
  \}" :: forall eff. Eff (dom :: DOM | eff) HTMLElement

type ECEffects eff = ( echartInit :: EC.EChartInit
                     , echartSetOption :: EC.EChartOptionSet
                     , echartDispose :: EC.EChartDispose
                     , dom :: DOM 
                     | eff)

-- | Create a component which responds to inputs of type `Option` by updating a
-- | chart component with those options.
-- |
-- | The first argument should be a unique identifier for this component.
-- |
-- | The second argument is a version number which can be used to detect when the 
-- | `update` function should be called.
chart :: forall eff res. String -> Int -> EC.Option -> Widget (ECEffects eff) res
chart id version opts = widget spec
  where
  spec = { value: version
         , name: "echarts"
         , id: id 
         , init: const init
         , update: update
         , destroy: destroy
         }

  init :: Eff (ECEffects eff) { context :: EC.EChart, node :: HTMLElement }
  init = do
    node <- createDiv
    ec <- EC.init Nothing node
    EC.setOption opts false ec
    return { context: ec, node: node }

  update :: Int -> Int -> EC.EChart -> HTMLElement -> Eff (ECEffects eff) (Maybe HTMLElement)
  update _ prevVersion ec _ = do
    when (version > prevVersion) $ 
      void $ EC.setOption opts false ec
    return Nothing

  destroy :: EC.EChart -> HTMLElement -> Eff (ECEffects eff) Unit
  destroy ec _ = EC.dispose ec
  
