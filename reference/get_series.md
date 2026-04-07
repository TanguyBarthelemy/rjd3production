# Extract all series from a SA-Item

Extracts all available time series (pre-adjustment, decomposition, and
final) from a seasonal adjustment item (`jsai`) inside a JDemetra+
workspace.

## Usage

``` r
get_series(x, ...)

# S3 method for class 'JD3_TRAMOSEATS_RSLTS'
get_series(x, name, ...)

# S3 method for class 'JD3_X13_RSLTS'
get_series(x, name, ...)

# S3 method for class 'jobjRef'
get_series(x, ...)
```

## Arguments

- x:

  The object to extract the series

- ...:

  Additional argument

- name:

  Name of the SA object

## Value

A `data.frame` with columns:

- `SAI`: name of the SAI,

- `series`: the type of series (e.g. `"y"`, `"sa"`, `"trend"`),

- `date`: observation dates,

- `value`: numeric values of the series.

## Details

`x` can be a Java SAI object, typically obtained via
[`rjd3workspace::jsap_sai()`](https://rjdverse.github.io/rjd3workspace/reference/jws_sap.html)
after opening and computing a workspace with
[`rjd3workspace::jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.html)
and
[`rjd3workspace::jws_compute()`](https://rjdverse.github.io/rjd3workspace/reference/jws_compute.html).

## Examples

``` r
# Create temporarily Workspaces

library("rjd3toolkit")
library("rjd3workspace")

# Demo workspace
jws <- create_ws_from_data(ABS)
jws_compute(jws)
jsap <- jws_sap(jws, 1L)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NullPointerException: Cannot invoke "jdplus.sa.base.api.SaEstimation.getQuality()" because "this.estimation" is null
jsai <- jsap_sai(jsap, 1L)
#> Error: object 'jsap' not found

df <- get_series(jsai)
#> Error: object 'jsai' not found
head(df)
#>                                               
#> 1 function (x, df1, df2, ncp, log = FALSE)    
#> 2 {                                           
#> 3     if (missing(ncp))                       
#> 4         .Call(C_df, x, df1, df2, log)       
#> 5     else .Call(C_dnf, x, df1, df2, ncp, log)
#> 6 }                                           
```
