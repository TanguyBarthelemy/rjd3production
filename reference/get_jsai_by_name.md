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
# Create temporarily Workspaces

library("rjd3toolkit")
library("rjd3workspace")

# Demo workspace
jws <- create_ws_from_data(ABS)
jws_compute(jws)
jsap <- jws_sap(jws, 1L)
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NullPointerException: Cannot invoke "jdplus.sa.base.api.SaEstimation.getQuality()" because "this.estimation" is null

jsai <- get_jsai_by_name(jws, "X0.2.09.10.M")
df <- get_series(jsai)
#> Error in UseMethod("get_series", x): no applicable method for 'get_series' applied to an object of class "NULL"
head(df)
#>                                               
#> 1 function (x, df1, df2, ncp, log = FALSE)    
#> 2 {                                           
#> 3     if (missing(ncp))                       
#> 4         .Call(C_df, x, df1, df2, log)       
#> 5     else .Call(C_dnf, x, df1, df2, ncp, log)
#> 6 }                                           
```
