module Options (options) where

import Prelude

import Data.Maybe (Maybe(..))
import ECharts.Axis as EA
import ECharts.Common (Sort(..))
import ECharts.Coords (XPos(..), YPos(..))
import ECharts.Item.Data (ItemData(..), dataDefault)
import ECharts.Item.Value (ItemValue(..))
import ECharts.Legend as EL
import ECharts.Options (Option(..), optionDefault)
import ECharts.Series as ES
import ECharts.Toolbox as ETB
import ECharts.Tooltip as ETT

simpleData ∷ Number → ItemData
simpleData = Value <<< Simple

lineOptions ∷ Option
lineOptions = Option $ optionDefault {
  tooltip = Just $ ETT.Tooltip ETT.tooltipDefault {trigger = Just ETT.TriggerAxis},
  legend = Just $ EL.Legend EL.legendDefault {
    x = Just XLeft,
    "data" = Just $ EL.legendItemDefault <$>
             ["email marketing", "affiliate advertising",
              "video ads", "direct access", "search engine"]
    },
  toolbox = Just $ ETB.Toolbox $ ETB.toolboxDefault {
    show = Just true,
    x = Just XRight,
    y = Just YBottom,
    feature = Just $ ETB.Feature $ ETB.featureDefault {
      mark = Just $ ETB.MarkFeature $ ETB.markFeatureDefault {show = Just true},
      dataView = Just $ ETB.DataViewFeature $ ETB.dataViewFeatureDefault {
        show = Just true,
        readOnly = Just false
        },
      magicType = Just $ ETB.MagicTypeFeature $ ETB.magicTypeFeatureDefault {
        show = Just true,
        "type" = Just [ETB.MagicLine, ETB.MagicBar, ETB.MagicStack, ETB.MagicTiled]
        },
      restore = Just $ ETB.RestoreFeature $ ETB.restoreFeatureDefault {
        show = Just true
        },
      saveAsImage = Just $ ETB.SaveAsImageFeature $ ETB.saveAsImageFeatureDefault {
        show = Just true
        }
      }
    },
  calculable = Just true,
  xAxis = Just $ EA.OneAxis $ EA.Axis $ EA.axisDefault {
    "type" = Just EA.CategoryAxis,
    boundaryGap = Just $ EA.CatBoundaryGap false,
    "data" = Just $ EA.CommonAxisData <$>
             ["Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday"]
    },
  yAxis = Just $ EA.OneAxis $ EA.Axis $ EA.axisDefault {
    "type" = Just EA.ValueAxis
    },

  series = Just $ Just <$> [
    ES.LineSeries {
       common: ES.universalSeriesDefault {
          name = Just "email marketing"
          },
       lineSeries: ES.lineSeriesDefault {
         stack = Just "total",
         "data" = Just $ simpleData <$> [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
         }
       },

    ES.LineSeries {
      common: ES.universalSeriesDefault {
         name = Just "affiliate advertising"
         },
      lineSeries: ES.lineSeriesDefault {
        "data" = Just $ simpleData <$> [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
        }
      },

    ES.LineSeries {
      common: ES.universalSeriesDefault {
         name = Just "video ads"
         },
      lineSeries: ES.lineSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
        }
      },

    ES.LineSeries {
      common: ES.universalSeriesDefault {
         name = Just "direct access"
         },
      lineSeries: ES.lineSeriesDefault {
        "data" = Just $ simpleData <$> [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0]
        }
      },

    ES.LineSeries {
      common: ES.universalSeriesDefault {
         name = Just "search engine"
         },
      lineSeries: ES.lineSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0]
        }
      }
    ]

  }


chordOptions ∷ Option
chordOptions = Option $ optionDefault {
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


indicator ∷ String → Number → EA.Indicator
indicator text max =
  EA.Indicator $ EA.indicatorDefault{text = Just text, max = Just max}

datPair ∷ Array Number → String → ItemData
datPair val name =
  Dat $ (dataDefault $ Many val) {name = Just name}

radarOptions ∷ Option
radarOptions = Option $ optionDefault {
  polar = Just $ [EA.Polar EA.polarDefault {
     indicator = Just [
        indicator "sales" 6000.0,
        indicator "Administration" 16000.0,
        indicator "IT" 30000.0,
        indicator "Development" 52000.0,
        indicator "Customer Support" 38000.0,
        indicator "Marketing" 25000.0
        ]
     }],
  series = Just $ Just <$> [
    ES.RadarSeries {
       common: ES.universalSeriesDefault{name = Just "budget vs spending"},
       radarSeries: ES.radarSeriesDefault{
         "data" = Just [
            datPair [4300.0, 10000.0, 28000.0, 35000.0, 50000.0, 19000.0] "Allocated",
            datPair [5000.0, 14000.0, 28000.0, 31000.0, 42000.0, 21000.0] "Actual"
            ]
         }
       }
    ]
  }



hloc ∷ Number → Number → Number → Number → ItemData
hloc o c l h = Value $ HLOC {
  h: h, l: l, o: o, c: c
  }

kOptions ∷ Option
kOptions = Option $ optionDefault {
  xAxis = Just $ EA.OneAxis $ EA.Axis EA.axisDefault {
     "type" = Just EA.CategoryAxis,
     "data" = Just $ EA.CommonAxisData <$>
              ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30"]
     },
  yAxis = Just $ EA.OneAxis $ EA.Axis EA.axisDefault {
    "type" = Just EA.ValueAxis,
    min = Just 2200.0,
    scale = Just true
    },
  series = Just $ Just <$> [
    ES.CandlestickSeries {
       common: ES.universalSeriesDefault,
       candlestickSeries: ES.candlestickSeriesDefault{
         "data" = Just $ [
            hloc 2320.26 2302.6 2287.3 2362.94,
            hloc 2300.0 2291.3 2288.26 2308.38,
            hloc 2295.35 2346.5 2295.35 2346.92,
            hloc 2347.22 2358.98 2337.35 2363.8,
            hloc 2360.75 2382.48 2347.89 2383.76
            ]
         }
       }
    ]
  }



simpleDat ∷ Number → String → ItemData
simpleDat val nam =
  Dat $ (dataDefault $ Simple val) {name = Just nam}

funnelOptions ∷ Option
funnelOptions = Option $ optionDefault {
  series = Just $ Just <$> [
     ES.FunnelSeries {
        common: ES.universalSeriesDefault,
        funnelSeries: ES.funnelSeriesDefault {
          "data" = Just $  [
             simpleDat 60.0 "foo",
             simpleDat 80.0 "bar",
             simpleDat 12.0 "baz",
             simpleDat 123.0 "quux"
             ],
          sort = Just Asc
          }
        }
     ]
  }



options ∷ Array Option
options = [ lineOptions
          , chordOptions
          , radarOptions
          , kOptions
          , funnelOptions
          ]
