# INSEE Regressors and Modelling Context

These functions allow to construct the standard regressors and modelling
context used by INSEE for seasonal adjustment:

- `create_french_calendar()` creates the French national calendar.

- `create_insee_regressors()` generates trading day regressors and
  leap-year effect (LY).

- `create_insee_regressors_sets()` organizes these regressors into
  standard sets (REG1, REG2, …, REG6, with or without LY).

- `create_insee_context()` combines the regressors and calendar into a
  `modelling_context` object that can be used directly with
  `rjd3toolkit`.

## Usage

``` r
create_french_calendar()

create_insee_regressors(
  start = c(1990L, 1L),
  frequency = 12L,
  length = 492L,
  s = NULL,
  cal = NULL
)

create_insee_regressors_sets(
  start = c(1990L, 1L),
  frequency = 12L,
  length = 492L,
  s = NULL,
  cal = NULL
)

create_insee_context(
  start = c(1990L, 1L),
  frequency = 12L,
  length = 492L,
  s = NULL
)
```

## Arguments

- start:

  \[[integer](https://rdrr.io/r/base/integer.html) vector\] Start period
  in the format `c(year, month)` (default `c(1990, 1)`).

- frequency:

  [integer](https://rdrr.io/r/base/integer.html) Series frequency
  (default `12L`).

- length:

  [integer](https://rdrr.io/r/base/integer.html) Series length (default
  `492L`).

- s:

  \[[numeric](https://rdrr.io/r/base/numeric.html) or NULL\] Optional
  argument for adjustment (passed to `rjd3toolkit`).

- cal:

  a calendar of class `JD3_CALENDAR`.

## Value

- `create_french_calendar()` returns a `national_calendar` object.

- `create_insee_regressors()` returns a matrix of regressors (working
  days

&nbsp;

- LY).

&nbsp;

- `create_insee_regressors_sets()` returns a list of regressor sets
  (`REG1`, `REG2`, …, `REG6`, with or without LY).

- `create_insee_context()` returns a `modelling_context` object.

## Examples

``` r
# 1. Create the French calendar
cal <- create_french_calendar()
cal
#> Holiday:
#>  - Fixed day: month=7, day=14
#>  - Fixed day: month=5, day=8 , from=1982-05-08
#>  - Prespecified holiday: event=NEWYEAR
#>  - Prespecified holiday: event=CHRISTMAS
#>  - Prespecified holiday: event=MAYDAY
#>  - Prespecified holiday: event=EASTERMONDAY
#>  - Prespecified holiday: event=ASCENSION
#>  - Prespecified holiday: event=WHITMONDAY
#>  - Prespecified holiday: event=ASSUMPTION
#>  - Prespecified holiday: event=ALLSAINTSDAY
#>  - Prespecified holiday: event=ARMISTICE
#> 
#> Mean correction: Yes

# 2. Generate regressors
regs <- create_insee_regressors(start = c(2000, 1), frequency = 12, length = 240)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class
head(regs)
#> Error: object 'regs' not found

# 3. Organize into standard sets
sets <- create_insee_regressors_sets(start = c(2000, 1), frequency = 12, length = 240)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
names(sets)
#> Error: object 'sets' not found

# 4. Build a complete context for rjd3toolkit
context <- create_insee_context(start = c(2000, 1), frequency = 12, length = 240)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): RcallMethod: cannot determine object class
context
#> Error: object 'context' not found
```
