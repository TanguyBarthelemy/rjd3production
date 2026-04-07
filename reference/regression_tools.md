# Manage regression components in JDemetra+ workspaces

These functions allow extracting, exporting, importing and assigning
regression components used in JDemetra+ workspaces.

## Usage

``` r
assign_outliers(jws, outliers)

assign_td(jws, td)

export_outliers(outliers, path = NULL, verbose = TRUE)

import_outliers(path, verbose = TRUE)

export_td(td, path = NULL, verbose = TRUE)

import_td(path, verbose = TRUE)

retrieve_outliers(
  jws,
  domain = TRUE,
  estimation = FALSE,
  point = FALSE,
  verbose = TRUE
)

retrieve_td(
  jws,
  domain = TRUE,
  estimation = FALSE,
  point = FALSE,
  verbose = TRUE
)
```

## Arguments

- jws:

  A Java Workspace object, as returned by
  [`rjd3workspace::jws_open()`](https://rjdverse.github.io/rjd3workspace/reference/jws_open.html)
  or
  [`rjd3workspace::jws_new()`](https://rjdverse.github.io/rjd3workspace/reference/jws_new.html).

- outliers:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] A data.frame
  created with retrieve_outliers or import_outliers. See Format section
  for more information about the format of this argument.

- td:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] A data.frame
  created by retrieve_td or import_td. See Format section for more
  information about the format of this argument.

- path:

  [character](https://rdrr.io/r/base/character.html) Path to a YAML file
  to read or write a table.

- verbose:

  Boolean. Print additional informations. Default is `TRUE`.

- domain:

  Boolean indicating if outliers should be extracted from the domain
  specification.

- estimation:

  Boolean indicating if outliers should be extracted from the estimation
  specification.

- point:

  Boolean indicating if outliers should be extracted from the point
  specification.

## Value

- `retrieve_outliers()` and `import_outliers()` returns data.frame
  representing the outliers.

- `retrieve_td()` and `import_td()` returns data.frame representing the
  trading days variables

- `export_outliers()` and `export_td()` functions invisibly return the
  path of the YAML file written.

- `assign_XXX()` functions invisibly return the updated workspace `jws`.

## Details

Two types of regression components are currently supported:

- **Outliers**

- **Trading-day regressors (TD)**

## Format

### Outliers table

Outliers are represented by a `data.frame` with **three columns**:

- `series` : name of the series in the workspace.

- `type` : type of outlier (`AO`, `LS`, `TC` or `SO`).

- `date` : date of the outlier in `YYYY-MM-DD` format.

These tables are typically created with `retrieve_outliers()` or
`import_outliers()`.

### Trading-day table

Trading-day specifications are represented by a `data.frame` with **two
columns**:

- `series` : name of the series in the workspace.

- `regs` : name of the trading-day regressor set to apply (e.g. `REG1`,
  `REG2`, ..., optionally with `LY`).

These tables are typically created with `retrieve_td()` or
`import_td()`.

## Workflow

The workflow typically follows these steps:

1.  Extract regression information from a workspace (`retrieve_XXX()`)

2.  Optionally export it to a YAML file (`export_XXX()`)

3.  Import it later from the YAML file (`import_XXX()`)

4.  Assign the regression specification to another workspace
    (`assign_XXX()`)

## Other

The assignment functions (`assign_XXX()`) modify the **first
SA-Processing** of the workspace.

Currently, regression information can be extracted (`retrieve_XXX()`)
from the point, estimation or domainSpec, while the assignment step
(`assign_XXX()`) is performed in both the domainSpec and the
estimationSpec.

## Examples

``` r
library("rjd3workspace")
file <- system.file("workspaces", "workspace_test.xml",
                    package = "rjd3workspace")
jws <- jws_open(file)

## Outliers

# Read all the outliers from a workspace
outs <- retrieve_outliers(jws)
#> Série RF0812, 1/5
#> Série RF0893, 2/5
#> Série RF0899, 3/5
#> Série RF1011, 4/5
#> Série RF1012, 5/5

# Export outliers
path_outs <- tempfile(pattern = "outliers-table", fileext = ".yaml")
export_outliers(outs, path_outs)
#> The outliers table will be written at  /tmp/Rtmp2J5Jca/outliers-table295262331d37.yaml 

# Import outliers from a file
outs2 <- import_outliers(path_outs)
#> The outliers table will be read at  /tmp/Rtmp2J5Jca/outliers-table295262331d37.yaml 

# Assign the outliers to a WS
assign_outliers(jws = jws, outliers = outs2)
#> Série RF0812, 1/5
#> Série RF0893, 2/5
#> Série RF0899, 3/5
#> Série RF1011, 4/5
#> Série RF1012, 5/5


## Trading day workflow

# Read all the td variables from a workspace
td <- retrieve_td(jws)
#> Série RF0812, 1/5
#> Série RF0893, 2/5
#> Série RF0899, 3/5
#> Série RF1011, 4/5
#> Série RF1012, 5/5

# Export td variables
path_td <- tempfile(pattern = "td-table", fileext = ".yaml")
export_td(td, path_td)
#> The td table will be written at  /tmp/Rtmp2J5Jca/td-table2952214b17d2.yaml 

# Import td variable from a file
td2 <- import_td(path_td)
#> The td table will be read at  /tmp/Rtmp2J5Jca/td-table2952214b17d2.yaml 

# Assign the td variables to a WS
#assign_td(jws = jws, td = td2)
```
