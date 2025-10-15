#' @title Manage outliers from Demetra+ workspaces
#'
#' @description
#' These functions allow extracting, exporting and importing outliers
#' detected in a Demetra+ workspace:
#'
#' - [retrieve_outliers()] extracts outliers from a `.xml` workspace.
#' - [export_outliers()] saves extracted outliers into a YAML file.
#' - [import_outliers()] loads outliers back from a YAML file.
#'
#' They are useful for archiving and reusing regression outliers detected
#' during seasonal adjustment workflows.
#'
#' @param ws_path [\link[base]{character}] Path to a Demetra+ workspace file
#' (usually with extension `.xml`).
#' @param x [list] A list of outliers, as returned by
#' [retrieve_outliers()].
#' @param ws_name [\link[base]{character}] The name of the workspace,
#' used to build default YAML filenames.
#' @param path [\link[base]{character}] Path to a YAML file to write to
#' or read from. If missing, defaults to
#' `"model/outliers_<ws_name>.yaml"`.
#' @param verbose [\link[base]{logical}] Whether to print informative
#' messages (default: `TRUE`).
#'
#' @return
#' - `retrieve_outliers()` returns a named list where each element
#'   corresponds to a series in the workspace and contains the names
#'   of detected outliers.
#' - `export_outliers()` invisibly returns the path of the YAML file
#'   written.
#' - `import_outliers()` returns a list of outliers read from YAML.
#'
#' @examples
#' \dontrun{
#' # Example workflow:
#' ws_file <- "path/to/workspace.xml"
#'
#' # 1. Retrieve outliers from a workspace
#' outliers <- retrieve_outliers(ws_file)
#'
#' # 2. Export them to a YAML file
#' export_outliers(outliers, ws_name = "workspace1")
#'
#' # 3. Import them back later
#' imported <- import_outliers(x = NULL, ws_name = "workspace1")
#' }
#'
#' @name outliers_tools
#' @export
retrieve_outliers <- function(ws_path) {
    jws <- rjd3workspace::jws_open(file = ws_path)
    ws <- read_workspace(jws, compute = FALSE)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()

    sap <- ws[["processing"]][[1L]]
    ps_outliers <- list()

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]
        cat(paste0("Série ", series_name, ", ", id_sai, "/", length(sap), "\n"))

        sai <- sap[[id_sai]]
        regression_section <- sai[["domainSpec"]][["regarima"]][["regression"]]

        outliers <- regression_section[["outliers"]] |> unique()
        ps_outliers[[series_name]] <- sapply(X = outliers, FUN = `[[`, "name")
    }

    return(ps_outliers)
}

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

#' @rdname outliers_tools
#' @export
import_outliers <- function(x, ws_name, path) {
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
