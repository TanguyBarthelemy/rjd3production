#' @title Assign outliers to a Demetra+ workspace
#'
#' @description
#' This function updates a Demetra+ workspace (`.xml`) by inserting
#' pre-specified outliers into the `domainSpec` of each seasonal adjustment
#' model (SAI).
#' Typically, the list of outliers is created with [retrieve_outliers()] and
#' stored/exported with [export_outliers()].
#'
#' @param outliers [\link[base]{list}] A named list where each element corresponds
#' to a series in the workspace.
#' Each element should be a character vector of outlier specifications
#' (e.g. `"AO (2020-03)"`, `"LS (2008-09)"`).
#'
#' @param ws_path [\link[base]{character}] Path to a Demetra+ workspace file
#' (usually with extension `.xml`).
#'
#' @details
#' For each series in the workspace:
#' - The function looks up the corresponding entry in the `outliers` list.
#' - If outliers are specified, they are split into **type** (`AO`, `LS`, `TC`, …)
#'   and **date** (e.g. `"2020-03"`) using regex patterns.
#' - These outliers are then added into the `domainSpec` of the model using
#'   [add_outlier()].
#'
#' The modified workspace is saved **in place** (argument `replace = TRUE`).
#'
#' @return
#' Invisibly returns `NULL`. The workspace file at `ws_path` is updated on disk
#' with the new outliers.
#'
#' @seealso
#' - [retrieve_outliers()] to extract outliers from an existing workspace.
#' - [export_outliers()] / [import_outliers()] to manage YAML files of outliers.
#'
#' @examples
#' \dontrun{
#' # Retrieve outliers from an existing workspace
#' outs <- retrieve_outliers("workspace.xml")
#'
#' # Reapply them to another workspace
#' affect_outliers(outliers = outs, ws_path = "workspace.xml")
#' }
#'
#' @export
affect_outliers <- function(outliers, ws_path) {
    jws <- .jws_open(file = ws_path)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    jsap <- jws_sap(jws, 1L)

    for (id_sai in seq_len(.jsap_sa_count(jsap))) {
        jsai <- .jsap_sa(jsap, idx = id_sai)
        series_name <- .jsa_name(jsai)
        cat(paste0(
            "Série ",
            series_name,
            ", ",
            id_sai,
            "/",
            .jsap_sa_count(jsap),
            "\n"
        ))

        # Outliers
        outliers_series <- outliers[[series_name]]

        # Création de la spec
        new_domainSpec <- domainSpec <- sai$domainSpec

        if (!is.null(outliers_series) && length(outliers_series) > 0L) {
            new_domainSpec <- domainSpec |>
                add_outlier(
                    type = outliers_type_pattern |>
                        gregexpr(text = outliers_series) |>
                        regmatches(x = outliers_series) |>
                        do.call(what = c),
                    date = date_pattern |>
                        gregexpr(text = outliers_series) |>
                        regmatches(x = outliers_series) |>
                        do.call(what = c)
                )
        }

        set_domain_specification(
            jsap = jsap,
            idx = id_sai,
            spec = new_domainSpec
        )
        set_name(jsap, idx = id_sai, name = series_name)
    }

    # Save WS automatique
    save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}
