
#' @importFrom yaml write_yaml
#' @rdname outliers_tools
#' @export
export_outliers <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("outliers_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The outliers will be written at ", path, "\n")
    }
    yaml::write_yaml(
        x = x,
        file = path
    )
    return(invisible(path))
}

#' @importFrom yaml read_yaml
#' @rdname outliers_tools
#' @export
import_outliers <- function(x, ws_name, path, verbose) {
    if (missing(path)) {
        file_name <- paste0("outliers_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The outliers will be read at ", path, "\n")
    }
    outliers <- yaml::read_yaml(
        file = path
    )
}

#' @importFrom yaml write_yaml
#' @name cjo_tools
#' @export
export_cjo <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("cjo_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The cjo will be written at ", path, "\n")
    }
    yaml::write_yaml(
        x = x,
        file = path
    )
    return(invisible(path))
}

#' @importFrom yaml read_yaml
#' @name cjo_tools
#' @export
import_cjo <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("cjo_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The cjo will be read at ", path, "\n")
    }
    cjo <- yaml::read_yaml(
        file = path
    )
}
