module Options (options) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable as T

import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import ECharts.Monad (DSL)

lineOptions ∷ DSL ETP.OptionI
lineOptions = do
  E.tooltip E.triggerAxis
  E.legend do
    E.leftLeft
    E.items
      $ map ET.strItem
        [ "email marketing"
        , "affiliate advertising"
        , "video ads"
        , "direct access"
        , "search engine"
        ]
  E.toolbox do
    E.shown
    E.leftRight
    E.topBottom
    E.feature do
      E.dataView do
        E.hidden
        E.readOnly false
      E.magicType do
        E.magics do
          E.magicStack
          E.magicTiled
          E.magicBar
          E.magicLine
      E.restore E.shown
      E.saveAsImage E.shown
  E.xAxis do
    E.axisType ET.Category
    E.disabledBoundaryGap
    E.items
      $ map ET.strItem
        [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]
  E.yAxis $ E.axisType $ ET.Value
  E.series do
    E.line do
      E.name "email marketing"
      E.items $ map ET.numItem [ 120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0 ]
    E.line do
      E.name "affiliate advertising"
      E.items $ map ET.numItem [ 220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0 ]
    E.line do
      E.name "video ads"
      E.stack "total"
      E.items $ map ET.numItem [ 150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0 ]
    E.line do
      E.name "direct access"
      E.items $ map ET.numItem [ 320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0 ]

    E.line do
      E.name "search engine"
      E.items $ map ET.numItem [ 820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0 ]


chordOptions ∷ DSL ETP.OptionI
chordOptions = do
  pure unit
{-  Option $ optionDefault {
  series = Just $ Just <$> [
     ES.ChordSeries {
        common: ES.universalSeriesDefault {
           name = Just "chord"
           },
        chordSeries: ES.chordSeriesDefault {
          sort = Just Asc,
          sortSub = Just Desc,
          showScale = Just true,
          showScaleText = Just true,
          "data" = Just [
            Label "group1",
            Label "group2",
            Label "group3",
            Label "group4"
            ],
          matrix = Just $  [
            [11975.0,  5871.0, 8916.0, 2868.0],
            [ 1951.0, 10048.0, 2060.0, 6171.0],
            [ 8010.0, 16145.0, 8090.0, 8045.0],
            [ 1013.0,   990.0,  940.0, 6907.0]
            ]
          }
        }
     ]
  }
-}

radarOptions ∷ DSL ETP.OptionI
radarOptions = do
  E.radar do
    E.indicators do
      E.indicator do
        E.name "sales"
        E.max 6000.0
      E.indicator do
        E.name "Administration"
        E.max 16000.0
      E.indicator do
        E.name "IT"
        E.max 30000.0
      E.indicator do
        E.name "Development"
        E.max 52000.0
      E.indicator do
        E.name "Customer Support"
        E.max 38000.0
      E.indicator do
        E.name "Marketing"
        E.max 25000.0
  E.series $ E.radarSeries do
    E.name "budget vs spending"
    E.buildItems do
      E.addItem do
        E.name "Allocated"
        E.values [ 4300.0, 10000.0, 28000.0, 35000.0, 50000.0, 19000.0 ]
      E.addItem do
        E.name "Actual"
        E.values [ 5000.0, 14000.0, 28000.0, 31000.0, 42000.0, 21000.0 ]

kOptions ∷ DSL ETP.OptionI
kOptions = do
  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30" ]
  E.yAxis do
    E.axisType ET.Value
    E.min 2200.0
    E.scale true
  E.series $ E.candlestick do
    E.buildItems
      $ T.traverse (E.addItem <<< E.values)
      [ [ 2320.26, 2302.6, 2287.3, 2362.94 ]
      , [ 2300.00, 2291.3, 2288.26, 2308.38 ]
      , [ 2295.35, 2346.5, 2295.35, 2346.92 ]
      , [ 2347.22, 2358.98, 2337.35, 2363.8 ]
      , [ 2360.75, 2382.48, 2347.89, 2383.76 ]
      ]


funnelOptions ∷ DSL ETP.OptionI
funnelOptions = do
  E.series $ E.funnel do
    E.ascending
    E.buildItems do
      E.addItem do
        E.name "foo"
        E.value 60.0
      E.addItem do
        E.name "bar"
        E.value 80.0
      E.addItem do
        E.name "baz"
        E.value 12.0
      E.addItem do
        E.name "quux"
        E.value 123.0


options ∷ Array (DSL ETP.OptionI)
options =
  [ lineOptions
  , chordOptions
  , radarOptions
  , kOptions
  , funnelOptions
  ]
