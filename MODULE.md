# Module Documentation

## Module Halogen.ECharts


This module defines an adapter between Halogen's widget API and
the `purescript-echarts` library.

#### `ECEffects`

``` purescript
type ECEffects eff = (ref :: Ref, dom :: DOM, echartDispose :: EC.EChartDispose, echartSetOption :: EC.EChartOptionSet, echartInit :: EC.EChartInit | eff)
```


#### `EChartsContext`

``` purescript
newtype EChartsContext
```

The ECharts context, which should be passed to the initialization 
and post-render hooks.

#### `newContext`

``` purescript
newContext :: forall eff. Eff (ref :: Ref | eff) EChartsContext
```

Create a new ECharts context.

#### `init`

``` purescript
init :: forall eff. EChartsContext -> HTMLElement -> Eff (ECEffects eff) Unit
```

The initialization hook, which creates ECharts components.

#### `postRender`

``` purescript
postRender :: forall eff driver. EChartsContext -> HTMLElement -> driver -> Eff (ECEffects eff) Unit
```

The post-render hook, which updates any ECharts components.

#### `chart`

``` purescript
chart :: forall i. String -> Number -> EC.Option -> H.HTML i
```

Create a chart component

#### `optionIsAttribute`

``` purescript
instance optionIsAttribute :: A.IsAttribute EC.Option
```




