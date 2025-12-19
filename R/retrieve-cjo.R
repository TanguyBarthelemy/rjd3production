#' @title Manage working-day regressors (CJO) from Demetra+ workspaces
#'
#' @description
#' These functions allow extracting, exporting, and importing the
#' *calendrier jours ouvrés* (CJO) regressors used in Demetra+ workspaces:
#'
#' - [retrieve_cjo()] extracts the CJO specification from a `.xml` workspace.
#' - [export_cjo()] saves extracted CJO information into a YAML file.
#' - [import_cjo()] loads CJO information back from a YAML file.
#'
#' They are useful for documenting and reusing the regression settings
#' applied in seasonal adjustment workflows.
#'
#' @param ws_path [\link[base]{character}] Path to a Demetra+ workspace
#' file (usually with extension `.xml`).
#' @param x [\link[base]{list} | \link[base]{data.frame}] An object containing
#' the CJO information, typically the output of [retrieve_cjo()].
#' @param ws_name [\link[base]{character}] The name of the workspace,
#' used to build default YAML filenames.
#' @param path [\link[base]{character}] Path to a YAML file to write to
#' or read from. If missing, defaults to
#' `"model/cjo_<ws_name>.yaml"`.
#' @param verbose [\link[base]{logical}] Whether to print informative
#' messages (default: `TRUE`).
#'
#' @return
#' - `retrieve_cjo()` returns a [data.frame] with columns:
#'   - `series`: series names,
#'   - `regs`: CJO regressor specification (`REG1`, `REG2`, …, `REG6`,
#'     with or without `_LY`),
#'   - `series_span`: currently empty but reserved for series span.
#' - `export_cjo()` invisibly returns the path of the YAML file written.
#' - `import_cjo()` returns a list or data structure read from YAML.
#'
#' @examples
#' \dontrun{
#' ws_file <- "path/to/workspace.xml"
#'
#' # 1. Retrieve CJO specification
#' cjo <- retrieve_cjo(ws_file)
#'
#' # 2. Export to YAML
#' export_cjo(cjo, ws_name = "workspace1")
#'
#' # 3. Import back from YAML
#' imported <- import_cjo(x = NULL, ws_name = "workspace1")
#' }
#'
#' @importFrom rjd3workspace jws_open read_workspace
#' @importFrom tools file_path_sans_ext
#' @name cjo_tools
#' @export
retrieve_cjo <- function(ws_path) {
    jws <- rjd3workspace::jws_open(file = ws_path)
    ws <- rjd3workspace::read_workspace(jws, compute = FALSE)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    sap <- ws[["processing"]][[1L]]

    cjo <- data.frame(
        series = names(sap),
        regs = character(length(sap)),
        series_span = character(length(sap)),
        stringsAsFactors = FALSE
    )

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]
        cat(paste0("Série ", series_name, ", ", id_sai, "/", length(sap), "\n"))

        sai <- sap[[id_sai]]
        regression_section <- sai[["domainSpec"]][["regarima"]][["regression"]]
        regressors <- regression_section[["td"]][["users"]]

        regs_cjo <- "Pas_CJO"
        if (any(grepl(pattern = "REG1", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG1"
        } else if (any(grepl(pattern = "REG5", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG5"
        } else if (any(grepl(pattern = "REG2", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG2"
        } else if (any(grepl(pattern = "REG3", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG3"
        } else if (any(grepl(pattern = "REG6", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG6"
        }
        if (
            any(
                grepl(pattern = "LeapYear", x = regressors, fixed = TRUE) |
                    grepl(pattern = "LY", x = regressors, fixed = TRUE)
            )
        ) {
            regs_cjo <- paste0(regs_cjo, "_LY")
        }
        cjo[id_sai, "regs"] <- regs_cjo
    }

    return(cjo)
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
