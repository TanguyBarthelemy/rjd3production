#' @title Remove non-significant outliers from a JDemetra+ workspace
#'
#' @description
#' This function scans a JDemetra+ workspace (`.xml`) and removes
#' regression outliers whose p-values are above a given threshold.
#' Both the estimation specification and the domain specification are
#' updated accordingly, and the workspace file is saved in place.
#'
#' Typical use case: after automatic model estimation, outliers with
#' weak statistical significance (e.g. `p > 0.3`) are dropped to
#' simplify the regression specification.
#'
#' @param ws_path [\link[base]{character}] Path to a JDemetra+ workspace file
#' (usually with extension `.xml`).
#' @param threshold [\link[base]{numeric}] Maximum p-value for keeping
#' an outlier. Outliers with `Pr(>|t|) > threshold` are removed.
#' Default is `0.3`.
#'
#' @details
#' The function:
#' - iterates over all seasonal adjustment models (SAI) in the workspace,
#' - identifies regression outliers in the `regarima` specification,
#' - checks their p-values in the pre-processing regression summary,
#' - removes those with p-values above the threshold from both
#'   `estimationSpec` and, if present, `domainSpec`,
#' - re-saves the workspace file.
#'
#' @return
#' The function invisibly returns `NULL`, but it **modifies the workspace file
#' in place** (saved at the same location as `ws_path`).
#'
#' @examples
#' \dontrun{
#' # Remove non-significant outliers (p > 0.3) from a workspace
#' remove_non_significative_outliers("workspace.xml", threshold = 0.3)
#' }
#'
#' @importFrom rjd3workspace jws_open jws_compute jws_sap sap_sai_count jsap_sai read_sai sai_name set_specification set_domain_specification set_name save_workspace
#' @importFrom rjd3toolkit remove_outlier
#' @importFrom tools file_path_sans_ext
#' @export
remove_non_significative_outliers <- function(ws_path, threshold = 0.3) {
    ws_name <- basename(ws_path) |> tools::file_path_sans_ext()
    cat("\n\U1F3F7 WS ", ws_name, "\n")
    jws <- rjd3workspace::jws_open(file = ws_path)
    rjd3workspace::jws_compute(jws)
    jsap <- rjd3workspace::jws_sap(jws, 1L)

    nb_sai <- rjd3workspace::sap_sai_count(jsap)

    for (id_sai in seq_len(nb_sai)) {
        cat("\U1F4CC SAI n\UB0", id_sai, "\n")
        jsai <- rjd3workspace::jsap_sai(jsap, idx = id_sai)
        sai <- rjd3workspace::read_sai(jsai)
        series_name <- rjd3workspace::sai_name(jsai)

        new_estimationSpec <- estimationSpec <- sai$estimationSpec
        new_domainSpec <- domainSpec <- sai$domainSpec

        outliers <- estimationSpec$regarima$regression$outliers
        outliers_domain <- domainSpec$regarima$regression$outliers
        outliers_name_domain <- lapply(
            X = outliers_domain,
            FUN = \(outlier) paste0(outlier$code, " (", outlier$pos, ")")
        ) |>
            do.call(what = c)

        xregs <- summary(sai)$preprocessing$xregs
        outliers_to_remove <- NULL

        for (id_out in seq_along(outliers)) {
            outlier <- outliers[[id_out]]
            outlier_name <- paste0(outlier$code, " (", outlier$pos, ")")

            if (
                outlier_name %in%
                    rownames(xregs) &&
                    !is.na(xregs[outlier_name, "Pr(>|t|)"]) &&
                    xregs[outlier_name, "Pr(>|t|)"] > threshold
            ) {
                cat("\U274C Suppression de l'outlier :", outlier_name, "\n")

                new_estimationSpec <- new_estimationSpec |>
                    rjd3toolkit::remove_outlier(type = outlier$code, date = outlier$pos)
                if (outlier_name %in% outliers_name_domain) {
                    new_domainSpec <- new_domainSpec |>
                        rjd3toolkit::remove_outlier(type = outlier$code, date = outlier$pos)
                    cat("L'outlier est dans la domainSpec.\n")
                }
                outliers_to_remove <- c(outlier_name, outliers_to_remove)
            }
        }

        rjd3workspace::set_specification(jsap, id_sai, new_estimationSpec)
        rjd3workspace::set_domain_specification(jsap, id_sai, new_domainSpec)
        rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
    }

    cat("\U1F4BE Saving WS file\n")
    rjd3workspace::save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}
