# Compare series across workspaces

Reads multiple JDemetra+ workspaces and extracts comparable series (by
SAI and series type), returning them in a tidy format. This is
particularly useful to compare results across different specifications
(e.g. RSA3 vs RSA5).

## Usage

``` r
compare(..., series_names)
```

## Arguments

- ...:

  [character](https://rdrr.io/r/base/character.html) Workspace file
  paths.

- series_names:

  [character](https://rdrr.io/r/base/character.html) Vector of SAI names
  to compare.

## Value

A `data.frame` with columns:

- `ws`: workspace name (derived from file basename),

- `SAI`: SAI name,

- `series`: type of series,

- `date`: observation date,

- `value`: numeric value.

## Examples

``` r
library("rjd3toolkit")
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad
library("rjd3x13")
#> 
#> Attaching package: ‘rjd3x13’
#> The following object is masked from ‘package:grDevices’:
#> 
#>     x11
library("rjd3workspace")

# Two demo workspaces (RSA3 and RSA5)
jws_rsa3 <- create_ws_from_data(ABS, x13_spec("rsa3"))
jws_rsa5 <- create_ws_from_data(ABS, x13_spec("rsa5"))

path_rsa3 <- tempfile(pattern = "ws-rsa3", fileext = ".xml")
path_rsa5 <- tempfile(pattern = "ws-rsa5", fileext = ".xml")

save_workspace(jws_rsa3, file = path_rsa3)
save_workspace(jws_rsa5, file = path_rsa5)

df <- compare(path_rsa3, path_rsa5, series_names = "X0.2.09.10.M")
#> Error in .jcall(jsai, "Ljdplus/sa/base/api/SaDefinition;", "getDefinition"): java.lang.NullPointerException
head(df)
#>                                               
#> 1 function (x, df1, df2, ncp, log = FALSE)    
#> 2 {                                           
#> 3     if (missing(ncp))                       
#> 4         .Call(C_df, x, df1, df2, log)       
#> 5     else .Call(C_dnf, x, df1, df2, ncp, log)
#> 6 }                                           
```
