module Halogen.ECharts
       ( echarts
       , EChartsState()
       , EChartsQuery(..)
       , initialEChartsState
       , EChartsEffects()
       ) where

import Prelude

import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())
import Control.Monad.Eff.Ref (Ref(), REF(), readRef, modifyRef, writeRef, newRef)
import Control.Monad.Maybe.Trans
import Control.Plus (empty)
import CSS.Geometry (width, height)
import CSS.Size (px)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types ( HTMLElement()
                      , htmlDocumentToParentNode
                      , htmlElementToParentNode
                      , htmlElementToElement)
import DOM.HTML.Window (document)
import DOM.Node.Element (getAttribute, setAttribute)
import DOM.Node.Node (removeChild, appendChild)
import DOM.Node.NodeList (length, item)
import DOM.Node.ParentNode (querySelectorAll, querySelector)
import DOM.Node.Types (elementToNode, NodeList(), Node(), ParentNode(), Element())
import Data.Array (singleton)
import Data.Date (Now(), nowEpochMilliseconds)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Nullable (toMaybe)
import Data.StrMap (StrMap(), insert, lookup)
import Data.StrMap as Sm
import Data.Time (Milliseconds(..))
import ECharts.Chart as Ec
import ECharts.Effects ( ECHARTS_INIT()
                       , ECHARTS_OPTION_SET()
                       , ECHARTS_DISPOSE()
                       , ECHARTS_RESIZE()
                       , ECHARTS_REFRESH()
                       , ECHARTS_CLEAR()
                       )
import ECharts.Options as Ec
import Halogen hiding (Prop())
import Halogen.HTML as H
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core (Prop(..), attrName)
import Halogen.HTML.Properties as P
import Unsafe.Coerce (unsafeCoerce)

-- | Attribute that contain unique key for component instance
dataEChartsKey :: forall i. String -> Prop i
dataEChartsKey = Attr Nothing (attrName "data-echarts-key")

-- | Foreig global state, it caches elements involved in chart render
foreign import memo :: Ref (StrMap {inst :: String, el :: Element})
-- | get `dataset` property of element
foreign import dataset :: forall e . Node -> Eff (dom :: DOM|e) (StrMap String)

-- | Memoize elements involved in chart render. In module level effectful state
memoChartElement
  :: forall e. String -> HTMLElement -> Eff (ref :: REF, dom :: DOM |e) Unit
memoChartElement k v =
  -- If `MaybeT` computation succeeded then take memo and put it to state
  maybe (pure unit) (modifyRef memo <<< insert k) =<< runMaybeT do
    -- try to take element with chart
    chartEl <- MaybeT $ map toMaybe
               $ querySelector "div" $ htmlElementToParentNode v
    -- try to take attribute used echarts like unique key
    chartInstance <- MaybeT $ map toMaybe
                     $ getAttribute "_echarts_instance_" $ htmlElementToElement v
    pure {inst: chartInstance, el: chartEl}


-- | Take memoized elements, remove them from parents and append to their
-- | true parents.
rearrange :: forall e. Eff (dom :: DOM, ref :: REF|e) Unit
rearrange = do
  -- We will flip `memo` content with `next` after `rearrange'`
  next <- newRef Sm.empty
  -- Until last component
  rearrange' next 0
  where
  rearrange'
    :: Ref (StrMap {inst :: String, el :: Element})
    -> Int -> Eff (dom :: DOM, ref :: REF|e) Unit
  rearrange' next n = do
    -- Get all components
    echartsContainers <- window
                         >>= document
                         >>= querySelectorAll "[data-echarts-key]"
                         <<< htmlDocumentToParentNode
    -- Count them
    count <- length echartsContainers
    if n >= count
      -- we've processed all components, let's remove unneeded keys from memo
      -- by flipping refs content
      then readRef next >>= writeRef memo
      else do
      -- move one component
      moveOne next n echartsContainers
      -- continue
      rearrange' next $ n + one

  moveOne :: Ref (StrMap {inst :: String, el :: Element})
          -> Int -> NodeList -> Eff (dom :: DOM, ref :: REF|e) Unit
  moveOne next ix containers = void $ runMaybeT do
      -- try to take component element
      container <- MaybeT $ map toMaybe $ item ix containers
      -- try to find chart element
      lift do mbel <- map (toMaybe >>> map elementToNode)
                        $ querySelector "div"
                        $ nodeToParentNode container
              -- and remove if it exists
              maybe (pure unit) (void <<< flip removeChild container) mbel
      datas <- lift $ dataset container
      -- get key to take memoized el and _echarts_instance_
      key <- maybe empty pure $ lookup "echartsKey" datas
      m <- lift $ readRef memo
      r <- maybe empty pure $ lookup key m
      -- append element, set attribute
      lift do
        appendChild (elementToNode r.el) container
        setAttribute "_echarts_instance_" r.inst $ nodeToElement container
        -- Everything works, this el/chart is exists, let's write it to new ref
        modifyRef next $ insert key r

  -- `Node` and `ParentNode` runtime representations are same. And if
  -- `Node` has no children, then `querySelector "div"` will return null.
  -- we catch it in `MaybeT`
  nodeToParentNode :: Node -> ParentNode
  nodeToParentNode = unsafeCoerce

  -- Used only in `container` and `container` is definitely an `Element`
  -- because we've taken its children before.
  nodeToElement :: Node -> Element
  nodeToElement = unsafeCoerce

genKey :: forall e. Eff (now :: Now, random :: RANDOM|e) String
genKey = do
  rn1 <- random
  rn2 <- random
  (Milliseconds time) <- nowEpochMilliseconds
  pure
    $  show rn1
    <> show time
    <> show rn2

type EChartsState =
  { option :: Maybe Ec.Option
  , chart :: Maybe Ec.EChart
  , width :: Int
  , height :: Int
  , key :: Maybe String
  }


initialEChartsState :: Int -> Int -> EChartsState
initialEChartsState w h =
  { option: Nothing
  , chart: Nothing
  , width: w
  , height: h
  , key: Nothing
  }

data EChartsQuery a
  = Set Ec.Option a
  | Resize a
  | Refresh a
  | Clear a
  | Dispose a
  | Init HTMLElement a
  | Quit HTMLElement a
  | SetHeight Int a
  | SetWidth Int a
  | GetOptions (Maybe Ec.Option -> a)
  | GetWidth (Int -> a)
  | GetHeight (Int -> a)

type EChartsEffects e = ( echartInit :: ECHARTS_INIT
                        , echartSetOption :: ECHARTS_OPTION_SET
                        , echartDispose :: ECHARTS_DISPOSE
                        , echartResize :: ECHARTS_RESIZE
                        , echartRefresh :: ECHARTS_REFRESH
                        , echartClear :: ECHARTS_CLEAR
                        , dom :: DOM
                        , random :: RANDOM
                        , now :: Now
                        , ref :: REF
                        | e)

echarts :: forall e. Component EChartsState EChartsQuery (Aff (EChartsEffects e))
echarts = component render eval

render :: EChartsState -> ComponentHTML EChartsQuery
render state =
  H.div ([ P.initializer \el -> action (Init el)
         , P.finalizer \el -> action (Quit el)
         , style do
              height $ px $ toNumber state.height
              width $ px $ toNumber state.width
         ]
         <>
         maybe [ ] (singleton <<< dataEChartsKey) state.key
        )
  [ ]


eval :: forall e.
        Eval EChartsQuery EChartsState EChartsQuery (Aff (EChartsEffects e))
eval (Set opts next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> do
      chart' <- liftEff' $ Ec.setOption opts true chart
      modify _{ chart = pure chart'
              , option = pure opts
              }
  pure next
eval (Resize next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.resize chart
  pure next
eval (Refresh next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.refresh chart
  pure next
eval (Clear next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.clear chart
  pure next
eval (Dispose next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.dispose chart
  pure next
eval (Init el next) = do
  state <- get
  when (isNothing state.key) do
    key <- liftEff' genKey
    modify _{key = pure key}
  when (isNothing state.chart) do
    chart <- liftEff' $ Ec.init Nothing el
    modify _{chart = pure chart}
    liftH $ pure unit
    mbk <- gets _.key
    case mbk of
      Nothing -> pure unit
      Just key ->
        liftEff' $ memoChartElement key el
  pure next
eval (Quit el next) = do
  liftEff' rearrange
  pure next
eval (SetHeight h next) = do
  modify _{height = h}
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.resize chart
  pure next
eval (SetWidth w next) = do
  modify _{width = w}
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.resize chart
  pure next
eval (GetOptions continue) = do
  opts <- gets _.option
  pure $ continue opts
eval (GetWidth continue) = do
  map continue $ gets _.width
eval (GetHeight continue) = do
  map continue $ gets _.height
