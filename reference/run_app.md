# Run the Shiny comparison app

Launches an interactive Shiny application to explore and compare
seasonal adjustment results stored in a `data.frame` returned by
[`compare()`](https://inseefr.github.io/rjd3production/reference/compare.md).

## Usage

``` r
run_app(data, ...)
```

## Arguments

- data:

  A `data.frame` returned by
  [`compare()`](https://inseefr.github.io/rjd3production/reference/compare.md),
  containing the columns `ws`, `SAI`, `series`, `date`, and `value`.

- ...:

  Additional arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

Runs a Shiny app in the R session (no return value).

## Examples

``` r
# Create temporary Workspaces

library("rjd3toolkit")
library("rjd3x13")
library("rjd3workspace")

# Two demo workspaces (RSA3 and RSA5)
jws_rsa3 <- create_ws_from_data(ABS, x13_spec("rsa3"))
jws_rsa5 <- create_ws_from_data(ABS, x13_spec("rsa5"))

path_rsa3 <- tempfile(pattern = "ws-rsa3", fileext = ".xml")
path_rsa5 <- tempfile(pattern = "ws-rsa5", fileext = ".xml")

save_workspace(jws_rsa3, file = path_rsa3)
save_workspace(jws_rsa5, file = path_rsa5)


# Compare the two workspace

df <- compare(path_rsa3, path_rsa5, series_names = "X0.2.09.10.M")
#> Error in .jcall(obj = jws, returnSig = "Ljdplus/sa/base/workspace/MultiProcessing;",     method = "getMultiProcessing", as.integer(idx - 1L)): java.lang.NullPointerException: Cannot invoke "jdplus.sa.base.api.SaEstimation.getQuality()" because "this.estimation" is null
head(df)
#>                                               
#> 1 function (x, df1, df2, ncp, log = FALSE)    
#> 2 {                                           
#> 3     if (missing(ncp))                       
#> 4         .Call(C_df, x, df1, df2, log)       
#> 5     else .Call(C_dnf, x, df1, df2, ncp, log)
#> 6 }                                           

# Launch the shiny app
if (interactive()) {
    run_app(df)
}
```
