module Test.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Random (randomInt, RANDOM())
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Data.Array ((!!), length, snoc, sort, reverse, head, filter)
import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Halogen
import Halogen.Util (appendToBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.ECharts
import Test.Options

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

type StateP =
  InstalledState State EChartsState Query EChartsQuery AffCharts Slot
type QueryP = Coproduct Query (ChildF Slot EChartsQuery)

type AppEffects = EChartsEffects ( err :: EXCEPTION
                                 , avar :: AVAR
                                 , console :: CONSOLE
                                 )

type AffCharts = Aff AppEffects

comp :: Component StateP QueryP AffCharts
comp = parentComponent' render eval peek

render :: RenderParent State EChartsState Query EChartsQuery AffCharts Slot
render state =
  H.div_
  $ [ H.h1_ [ H.text "purescript-halogen-echarts" ]
    , H.button [ E.onClick (E.input_ AddChart) ] [ H.text "Add chart" ]
    ]
  <> map renderOne state.arr
  where
  renderOne ix =
    H.div_
    [ H.div_ [ H.slot ix \_ -> { component: echarts
                               , initialState: initialEChartsState 400 300
                               }
             ]
    , H.button [ E.onClick (E.input_ (SetRandomOption ix))
               ]
      [ H.text "Set random option" ]
    , H.button [ E.onClick (E.input_ (RemoveChart ix))
               ]
      [ H.text "Remove" ]
    ]

eval :: EvalParent Query State EChartsState Query EChartsQuery AffCharts Slot
eval (SetRandomOption ix next) = do
  mbopt <- liftH $ liftEff' $ randomInArray options
  case mbopt of
    Nothing -> pure unit
    Just opt -> void $ query ix $ action (Set opt)
  pure next
eval (AddChart next) = do
  modify (\x -> x{arr = snoc x.arr (maybe 0 (add one) $ head $ reverse $ sort x.arr)})
  pure next
eval (RemoveChart ix next) = do
  modify (\x -> x{arr = filter (/= ix) x.arr})
  pure next

peek :: Peek (ChildF Int EChartsQuery)
        State EChartsState Query EChartsQuery AffCharts Int
peek (ChildF ix (Init _ _)) = do
  mbopt <- liftH $ liftEff' $ randomInArray options
  case mbopt of
    Nothing -> pure unit
    Just opt -> void $ query ix $ action (Set opt)
peek _ = pure unit

main :: Eff AppEffects Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI comp (installedState initialState)
  appendToBody app.node
