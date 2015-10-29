## Module Halogen.ECharts

#### `EChartsState`

``` purescript
type EChartsState = { option :: Maybe Option, chart :: Maybe EChart, width :: Int, height :: Int, key :: Maybe String }
```

#### `initialEChartsState`

``` purescript
initialEChartsState :: Int -> Int -> EChartsState
```

#### `EChartsQuery`

``` purescript
data EChartsQuery a
  = Set Option a
  | Resize a
  | Refresh a
  | Clear a
  | Dispose a
  | Init HTMLElement a
  | Quit HTMLElement a
  | SetHeight Int a
  | SetWidth Int a
  | GetOptions (Maybe Option -> a)
  | GetWidth (Int -> a)
  | GetHeight (Int -> a)
```

#### `EChartsEffects`

``` purescript
type EChartsEffects e = (echartInit :: ECHARTS_INIT, echartSetOption :: ECHARTS_OPTION_SET, echartDispose :: ECHARTS_DISPOSE, echartResize :: ECHARTS_RESIZE, echartRefresh :: ECHARTS_REFRESH, echartClear :: ECHARTS_CLEAR, dom :: DOM, random :: RANDOM, now :: Now, ref :: REF | e)
```

#### `echarts`

``` purescript
echarts :: forall e. Component EChartsState EChartsQuery (Aff (EChartsEffects e))
```


