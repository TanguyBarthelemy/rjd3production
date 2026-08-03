linters: all_linters(
    indentation_linter = lintr::indentation_linter(indent = 4L),
    line_length_linter = lintr::line_length_linter(80L),
    return_linter = NULL,
    library_call_linter = NULL,
    undesirable_function_linter = NULL
    )
encoding: "UTF-8"
exclpsions: list("renv", "packrat")

