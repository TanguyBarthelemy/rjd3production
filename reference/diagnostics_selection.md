# Diagnostics Extraction on Calendar Correction with different sets of regressors

These functions allow to extract diagnostics from X13-Arima models with
different sets of calendar regressors in order to evaluate different
specifications and select the most appropriate calendar regressors set
(with or without leap-year effect) to correct a given series.

## Usage

``` r
get_LY_info(smod, verbose = TRUE)

one_diagnostic(series, spec, context)

all_diagnostics(series, specs_set, context)

verif_LY(jeu, diags)

select_reg_one_series(series, name = "", specs_set = NULL, context = NULL, ...)
```

## Arguments

- smod:

  [list](https://rdrr.io/r/base/list.html) Result of
  [`summary()`](https://rdrr.io/r/base/summary.html) applied to an X13
  model.

- series:

  \[[ts](https://rdrr.io/r/stats/ts.html) or numeric\] Time series to
  analyse.

- spec:

  [list](https://rdrr.io/r/base/list.html) A X13 specification (from
  [`rjd3x13::x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.html)).

- context:

  [list](https://rdrr.io/r/base/list.html) Modelling context with
  regressors and calendars (from
  [`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html)).

- specs_set:

  \[[list](https://rdrr.io/r/base/list.html) or NULL\] List of X13
  specifications. If `NULL`, generated via
  [`create_specs_set()`](https://inseefr.github.io/rjd3production/reference/create_specs_set.md).

- jeu:

  [character](https://rdrr.io/r/base/character.html) Name of the tested
  regression set.

- diags:

  [data.frame](https://rdrr.io/r/base/data.frame.html) Diagnostics table
  produced by `all_diagnostics()`.

- name:

  [character](https://rdrr.io/r/base/character.html) Name of the series
  (for messages).

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

- `get_LY_info()` : A data.frame with `LY_coeff` and `LY_p_value`.

- `one_diagnostic()` : A data.frame with diagnostics for one
  specification.

- `all_diagnostics()` : A data.frame with diagnostics for all
  specifications.

- `verif_LY()` : Name of the chosen regression set (possibly without
  LY).

- `select_reg_one_series()` : Name of the selected regression set.

## Details

- `get_LY_info()` extracts coefficient and p-value of the leap-year (LY)
  effect.

- `one_diagnostic()` applies one X13 specification to a series and
  computes diagnostics.

- `all_diagnostics()` evaluates all specifications in a set and
  summarizes diagnostics.

- `verif_LY()` checks whether the leap-year effect should be kept or
  removed.

- `select_reg_one_series()` selects the best calendar regressors set for
  a single series.

## Examples

``` r
# Create a modelling context
my_context <- create_insee_context(s = AirPassengers)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class

# Generate specification sets
my_set <- create_specs_set(context = my_context)
#> Error: object 'my_context' not found

# Extract LY info
mod <- rjd3x13::x13(AirPassengers, spec = "RSA3")
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
rjd3production:::get_LY_info(summary(mod))
#> Error: object 'mod' not found

# Compute diagnostics for one spec
spec <- my_set[[8L]]
#> Error: object 'my_set' not found
rjd3production:::one_diagnostic(series = AirPassengers, spec, context = my_context)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): RcallMethod: cannot determine object class

# Compute diagnostics for all specs
rjd3production:::all_diagnostics(series = AirPassengers, specs_set = my_set, context = my_context)
#> Error: object 'my_set' not found

# Check whether LY should be removed
diags <- rjd3production:::all_diagnostics(
    series = AirPassengers,
    specs_set = my_set,
    context = my_context
)
#> Error: object 'my_set' not found
rjd3production:::verif_LY("REG6_LY", diags)
#> Error: object 'diags' not found

# Select regressions for one series
rjd3production:::select_reg_one_series(series = AirPassengers, context = my_context)
#> Error: object 'my_context' not found
```
