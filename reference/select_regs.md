# Select Calendar Regressors for One or Multiple Series

Applies the X13 regression selection procedure to one or more time
series. If multiple series are provided as columns of a matrix or
data.frame, each series is processed separately. The function returns
the selected set of regressors for each series.

## Usage

``` r
select_regs(series, context = NULL, ...)
```

## Arguments

- series:

  \[[ts](https://rdrr.io/r/stats/ts.html) or mts or matrix or
  [data.frame](https://rdrr.io/r/base/data.frame.html)\] A univariate
  time series (`ts`) or a multivariate series (columns as separate
  series).

- context:

  [list](https://rdrr.io/r/base/list.html) Modeling context created by
  [`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html).

- ...:

  Additional arguments passed to
  [`create_specs_set()`](https://inseefr.github.io/rjd3production/reference/create_specs_set.md)
  controlling the generation of X13 specifications. Possible arguments
  include:

  outliers

  :   Optional list of outliers with elements `type` (vector of types,
      e.g., "AO", "LS", "TC") and `date` (vector of dates).

  span_start

  :   Starting date of the estimation (character, format
      `"YYYY-MM-DD"`).

  ...

  :   Other arguments accepted by
      [`create_specs_set()`](https://inseefr.github.io/rjd3production/reference/create_specs_set.md).

## Value

A data.frame with two columns:

- series:

  Name of the series (column name if `series` is multivariate).

- reg_selected:

  Name of the selected regressor set.

## Examples

``` r
# Single series
select_regs(AirPassengers)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class

# Multiple series
select_regs(Seatbelts[, -8])
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

# Restrict regressors sets
my_context <- create_insee_context()
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class
my_context$variables <- my_context$variables[c("REG1", "REG1_LY", "REG6", "REG6_LY")]
#> Error: object 'my_context' not found
select_regs(Seatbelts[, -8], context = my_context)
#> Error: object 'my_context' not found
```
