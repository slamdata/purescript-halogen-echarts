module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt, RANDOM)

import Data.Array ((!!), length, snoc, sort, reverse, head, filter)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Options (options)

randomInArray ∷ ∀ e a. Array a → Eff (random ∷ RANDOM|e) (Maybe a)
randomInArray arr = do
  n ← randomInt 0 (length arr - 1)
  pure $ arr !! n

type State =
  { arr ∷ Array Int }


initialState ∷ ∀ a. a → State
initialState _ = { arr: [ ] }

data Query a
  = SetRandomOption Int a
  | AddChart a
  | RemoveChart Int a
  | HandleEChartsMessage Int EC.EChartsMessage a

type Slot = Int

type AppEffects = EC.EChartsEffects ( random ∷ RANDOM )

type AffCharts = Aff AppEffects

type HTML = H.ParentHTML Query EC.EChartsQuery Int AffCharts
type DSL = H.ParentDSL State Query EC.EChartsQuery Int Void AffCharts

comp ∷ H.Component HH.HTML Query Unit Void AffCharts
comp = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ State → HTML
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
    HH.div_
      [ HH.div_
          [ HH.slot ix (EC.echarts Nothing) ({width: 400, height: 300} /\ unit)
              (Just <<< H.action <<< HandleEChartsMessage ix)
          ]
      , HH.button
          [ HE.onClick (HE.input_ (SetRandomOption ix)) ]
          [ HH.text "Set random option" ]
      , HH.button
          [ HE.onClick (HE.input_ (RemoveChart ix)) ]
          [ HH.text "Remove" ]
      ]

eval ∷ Query ~> DSL
eval (SetRandomOption ix next) = do
  mbopt ← liftEff $ randomInArray options
  for_ mbopt \opt →
    void $ H.query ix $ H.action $ EC.Reset opt
  pure next
eval (AddChart next) = do
  H.modify (\x → x{arr = snoc x.arr (maybe 0 (add one) $ head $ reverse $ sort x.arr)})
  pure next
eval (RemoveChart ix next) = do
  H.modify (\x → x{arr = filter (_ /= ix) x.arr})
  pure next
eval (HandleEChartsMessage ix EC.Initialized next) = do
  mbopt ← liftEff $ randomInArray options
  for_ mbopt \opt →
    void $ H.query ix $ H.action $ EC.Set opt
  pure next

main ∷ Eff AppEffects Unit
main = runHalogenAff do
  body ← awaitBody
  runUI comp unit body
