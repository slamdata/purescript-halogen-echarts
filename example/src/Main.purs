module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (randomInt, RANDOM())

import Data.Array ((!!), length, snoc, sort, reverse, head, filter)
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural())

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import Options (options)

randomInArray :: forall e a.Array a -> Eff (random :: RANDOM|e) (Maybe a)
randomInArray arr = do
  n <- randomInt 0 (length arr - 1)
  pure $ arr !! n

type State =
  { arr :: Array Int
  }

initialState :: State
initialState = { arr: [ ] }

data Query a
  = SetRandomOption Int a
  | AddChart a
  | RemoveChart Int a

type Slot = Int

type StateP = H.ParentState State EC.EChartsState Query EC.EChartsQuery AffCharts Slot
type QueryP = Coproduct Query (H.ChildF Slot EC.EChartsQuery)

type AppEffects = EC.EChartsEffects
  ( err :: EXCEPTION
  , random :: RANDOM
  )

type AffCharts = Aff AppEffects

type HTML = H.ParentHTML EC.EChartsState Query EC.EChartsQuery AffCharts Slot
type DSL = H.ParentDSL State EC.EChartsState Query EC.EChartsQuery AffCharts Slot

comp :: H.Component StateP QueryP AffCharts
comp = H.parentComponent { render, eval, peek: Just peek }

render :: State -> HTML
render state =
  HH.div_
    $ [ HH.h1_ [ HH.text "purescript-halogen-echarts" ]
      , HH.button
          [ HE.onClick (HE.input_ AddChart) ]
          [ HH.text "Add chart" ]
      ]
    <> map renderOne state.arr
  where
  renderOne ix =
    HH.div
      [ HP.key ("echarts-" <> show ix) ]
      [ HH.div_
          [ HH.slot ix \_ ->
              { component: EC.echarts
              , initialState: EC.initialEChartsState 400 300
              }
          ]
      , HH.button
          [ HE.onClick (HE.input_ (SetRandomOption ix)) ]
          [ HH.text "Set random option" ]
      , HH.button
          [ HE.onClick (HE.input_ (RemoveChart ix)) ]
          [ HH.text "Remove" ]
      ]

eval :: Natural Query DSL
eval (SetRandomOption ix next) = do
  mbopt <- H.fromEff $ randomInArray options
  case mbopt of
    Nothing -> pure unit
    Just opt -> void $ H.query ix $ H.action (EC.Set opt)
  pure next
eval (AddChart next) = do
  H.modify (\x -> x{arr = snoc x.arr (maybe 0 (add one) $ head $ reverse $ sort x.arr)})
  pure next
eval (RemoveChart ix next) = do
  H.modify (\x -> x{arr = filter (/= ix) x.arr})
  pure next

peek :: forall x. H.ChildF Int EC.EChartsQuery x -> DSL Unit
peek (H.ChildF ix (EC.Init _)) = do
  mbopt <- H.fromEff $ randomInArray options
  case mbopt of
    Nothing -> pure unit
    Just opt -> void $ H.query ix $ H.action (EC.Set opt)
peek _ = pure unit

main :: Eff AppEffects Unit
main = runHalogenAff $ H.runUI comp (H.parentState initialState) =<< awaitBody
