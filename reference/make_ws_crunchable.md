# Make a workspace crunchable

Complete and replace the ts metadata of a WS to make it crunchable

## Usage

``` r
make_ws_crunchable(jws, verbose = TRUE)
```

## Arguments

- jws:

  The java representation of the workspace

- verbose:

  Boolean. Print additional informations.

## Value

A java workspace (as jws) but with new ts metadata

## Details

New metadata are added from temporary files created on the heap. Thus,
this operation is not intended to make the workspace crunchable in a
stable way over time, but rather for a short period of time for testing
purposes, in particular when we are sent a workspace without the raw
data.

## Examples

``` r
library("rjd3workspace")
library("rjd3x13")
#> Your java version is 17. 21 or higher is needed.
#> 
#> Attaching package: ‘rjd3x13’
#> The following object is masked from ‘package:grDevices’:
#> 
#>     x11

jws <- jws_new()
#> Error in jws_new(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.sa.base.workspace.Ws
jsap <- jws_sap_new(jws, "sap1")
#> Error: object 'jws' not found
add_sa_item(
    jsap = jsap,
    name = "series_3",
    x = AirPassengers,
    spec = x13_spec("RSA3")
)
#> Error in .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;",     "of", as.integer(freq), as.integer(start[1]), as.integer(start[2]),     as.double(s)): RcallMethod: cannot determine object class
jws <- make_ws_crunchable(jws)
#> Error in .jcall(jws, "I", "getMultiProcessingCount"): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/r/timeseries/TsUtility has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
