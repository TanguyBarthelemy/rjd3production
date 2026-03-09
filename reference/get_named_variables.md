# Retrieve all the auxiliary variables from a workspace

Lists all the variables in a modelling context.

## Usage

``` r
get_named_variables(context = NULL)
```

## Arguments

- context:

  a modelling context

## Value

a list with all the groups and named variables

## Examples

``` r
context_FR <- create_insee_context()
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsDomain;",     "of", as.integer(period), as.integer(startYear), as.integer(startPeriod),     as.integer(length)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
get_named_variables(context_FR)
#> Error: object 'context_FR' not found
```
