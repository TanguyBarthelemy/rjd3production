# Setup a production process of Seasonally Adjusted series, with JDemetra+

``` r

library("rjd3production")
```

The {rjd3production} package is useful for setting up a production
pipeline for seasonally adjusted time series.

Before creating a production pipeline, you must set up a working
environment as an R project. The
[`init_env()`](https://inseefr.github.io/rjd3production/reference/init_env.md)
function creates the following default structure:

- a `data/` folder: raw data
- a `Workspaces/` folder: workspaces
- an `output/` folder: the output time series, tables and graphs
- a `specs/` folder: workspace-specific specifications (calendar
  regressors, outliers, etc.)
- a `BQ/` folder: quality reports and decision files
- a DESCRIPTION file to manage the project’s dependencies
- a `.lintr` file for static code analysis (formatting best practices)
- a README.md file to describe the process
- a Git project structure

``` r

path_project <- tempfile(pattern = "my_sa_project")
init_env(path = path_project)
```

In this vignette, we will create a production pipeline using the ABS
dataset from the {rjd3toolkit} package. The dataset is also available as
the file `ABS.csv` at
/home/runner/work/\_temp/Library/rjd3providers/extdata/ABS.csv in the
{rjd3providers} package.

``` r

library("rjd3toolkit")
path_ABS <- system.file("extdata", "ABS.csv", package = "rjd3providers")
my_data <- ABS[, seq_len(3L)]
colnames(my_data) <- substr(colnames(my_data), start = 2L, stop = 12L)
```

## Selection of Calendar Regressors

If the time series are sensitive to calendar effects, these effects can
be corrected using calendar regressors.

To do this, refer to the `td-selection` vignette, which explains how to
select the optimal set of regressors for each time series and how to
generate the `td` table describing the correspondence between the time
series and the selected set of calendar regressors.

``` r

td <- select_td(my_data)
#> 
#> Série 0.2.09.10.M en cours... 1/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
#> 
#> Série 0.2.08.10.M en cours... 2/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
#> 
#> Série 0.2.07.10.M en cours... 3/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
```

## Creating a workspace

We will use the {rjd3workspace} package for functions related to the
creation and manipulation of workspaces.

``` r

library("rjd3workspace")
```

To create a new workspace, you can either create an empty one and
populate it, or create it directly from a dataset.

If you use external variables, regressors, or a custom calendar, be sure
to place all of these within a \`modeling context.

At INSEE, we use the
[`create_insee_context()`](https://inseefr.github.io/rjd3production/reference/insee_modelling.md)
function to create our `contexts`:

``` r

my_context <- create_insee_context(s = my_data[, 1L])
```

We will use the {rjd3x13} package to create the X13 specifications.

``` r

library("rjd3x13")
```

- Option 1: Create a workspace from scratch

``` r

jws <- jws_new(modelling_context = my_context)
jsap <- jws_sap_new(jws, "Nouveau SAP")
add_sa_item(
    jsap = jsap,
    name = "Première série",
    x = my_data[, 1L],
    spec = x13_spec()
)
add_sa_item(
    jsap = jsap,
    name = "Seconde série",
    x = my_data[, 2L],
    spec = x13_spec()
)
#... avec autant de commande que de séries
```

- Option 2: Create a workspace from a dataset

``` r

jws <- create_ws_from_data(my_data)
set_context(jws, create_insee_context(s = my_data))
```

If you want to use calendar regressors, you must assign the previously
selected set to each series:

``` r

jws_compute(jws)
assign_td(td = td, jws = jws, spec_type = c("Estimation", "Reference"))
#> Série 0.2.09.10.M, 1/3
#> Série 0.2.08.10.M, 2/3
#> Série 0.2.07.10.M, 3/3
```

Above, the regressors are specified in the reference specification
(Reference Spec) and in the estimation specification (Estimation Spec).
These concepts are explained
[here](https://prod.jdemetra.org/p1-overview#sec-concept-specification)
and [here](https://prod.jdemetra.org/p4-appendices#comp-specs).

Don’t forget to update the workspace metadata with the path to the raw
data:

``` r

add_raw_data_path(jws, path_ABS, delimiter = "COMMA")
```

Finally, we can save our workspace!

It will stop being just a Java Virtual Object and become a set of (XML)
files.

``` r

path_ws <- file.path(path_project, "Workspaces", "workspace_travail", "my_ws.xml")
save_workspace(jws, path_ws, replace = TRUE)
```

## Using Cruncher for Estimation and Output Generation

When using a workspace structure (benefit: having data that can be read
by the graphical interface), JDemetra+
[Cruncher](https://doc.jdemetra.org/t-production-tools-cruncher-qr)
allows you to automate two important operations:

1.  Updating the raw data from the data file

2.  Updating the seasonally adjusted data by taking into account new raw
    data points and/or revisions to the recent past. You can choose a
    refresh policy to update the seasonal adjustment model with varying
    degrees of constraint. For more details, see the
    [documentation](https://doc.jdemetra.org/a-rev-policies).
