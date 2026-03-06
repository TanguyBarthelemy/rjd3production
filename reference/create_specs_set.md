# Creating a set of X13 specifications

Builds a set of X13 specifications from a start date, a modelling
context (explanatory variables) and outliers (optional).

## Usage

``` r
create_specs_set(
  spec_0 = NULL,
  context = NULL,
  outliers = NULL,
  span_start = NULL
)
```

## Arguments

- spec_0:

  Basic specification

- context:

  [list](https://rdrr.io/r/base/list.html) Modeling context created by
  [`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html).

- outliers:

  \[[list](https://rdrr.io/r/base/list.html) or NULL\] Optional list
  with elements :

  - `type`: vector of outlier types (e.g. "AO", "LS", "TC")

  - `date`: vector of corresponding dates

- span_start:

  [character](https://rdrr.io/r/base/character.html) Estimation start
  date (format "YYYY-MM-DD").

## Value

A list of named X13 specifications (TD and variants).

## Examples

``` r
my_context <- create_insee_context()
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class
create_specs_set(context = my_context)
#> Error: object 'my_context' not found
```
