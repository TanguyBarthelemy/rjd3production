# Retrieve a SA-Item by its name

Searches a workspace for a seasonal adjustment item (SAI) whose name
matches the user-supplied string and returns the corresponding object.

## Usage

``` r
get_jsai_by_name(jws, series_name)
```

## Arguments

- jws:

  A Java Workspace object, as returned by
  [`rjd3workspace::jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.html)
  or
  [`rjd3workspace::jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.html).

- series_name:

  [character](https://rdrr.io/r/base/character.html) Name of the SAI to
  retrieve.

## Value

A Java Seasonal Adjustment Item object (`jsai`).

## Examples

``` r

library("rjd3toolkit")
library("rjd3workspace")
# \donttest{
# Demo workspace
jws <- create_ws_from_data(ABS)
jws_compute(jws)
jsap <- jws_sap(jws, 1L)

jsai <- get_jsai_by_name(jws, "X0.2.09.10.M")
df <- get_series(jsai)
#> [1] "a1"
#> [1] "a1a"
#> [1] "a1b"
#> [1] "a6"
#> [1] "a7"
#> [1] "a8"
#> [1] "a9"
#> [1] "d1"
#> [1] "d2"
#> [1] "d4"
#> [1] "d5"
#> [1] "d6"
#> [1] "d7"
#> [1] "d8"
#> [1] "d9"
#> [1] "d10"
#> [1] "d11"
#> [1] "d12"
#> [1] "d13"
#> [1] "d11final"
#> [1] "d12final"
#> [1] "d13final"
#> [1] "d16"
#> [1] "d18"
#> [1] "d11a"
#> [1] "d12a"
#> [1] "d16a"
#> [1] "d18a"
#> [1] "e1"
#> [1] "e2"
#> [1] "e3"
#> [1] "e11"
head(df)
#>            SAI series       date value
#> 1 X0.2.09.10.M     a1 1982-04-01 460.1
#> 2 X0.2.09.10.M     a1 1982-05-01 502.6
#> 3 X0.2.09.10.M     a1 1982-06-01 443.8
#> 4 X0.2.09.10.M     a1 1982-07-01 459.1
#> 5 X0.2.09.10.M     a1 1982-08-01 438.4
#> 6 X0.2.09.10.M     a1 1982-09-01 465.1
# }
```
