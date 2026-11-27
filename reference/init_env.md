# Initialize a seasonal adjustment project environment

This function creates a complete project structure for a seasonal
adjustment production workflow. It initializes an R project, sets up
useful directories, configuration files, and development tools.

The generated structure is designed for workflows based on the
'rjdverse'.

## Usage

``` r
init_env(path, open = FALSE)
```

## Arguments

- path:

  A character string. Path to the repository inside which the project
  will be created.

- open:

  Boolean. Should the project be opened in RStudio after creation?
  Default is `FALSE`.

## Value

The project path invisibly.

## Details

The final repository pointed at by `path` has to not yet exist as it
will be created.

## References

More information on SA production process structure
<https://prod.jdemetra.org/>

## Examples

``` r
project_path <- tempfile(pattern = "my-project")

if (interactive()) {
# Create a new project
init_env(path = project_path)
}
```
