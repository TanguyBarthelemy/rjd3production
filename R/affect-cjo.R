#' @title Assign CJO regressors to a Demetra+ workspace
#'
#' @description
#' This function updates a Demetra+ workspace (`.xml`) by assigning
#' user-defined trading day regressors (CJO) to each seasonal adjustment model
#' (SAI), based on an external classification (typically created with
#' [retrieve_cjo()]).
#'
#' The function modifies the `domainSpec` of each series by setting
#' `tradingdays` to `"UserDefined"` with the appropriate regressors
#' (`REG1` … `REG6`, optionally with `LY` for leap year).
#'
#' @param cjo [\link[base]{data.frame}] A data.frame with at least two columns:
#' - `series`: names of the series in the workspace.
#' - `regs`: the standard INSEE CJO set (`REG1`, `REG2`, …, `REG6`, with or without `_LY`).
#'
#' Typically this table is produced by [retrieve_cjo()].
#'
#' @param ws_path [\link[base]{character}] Path to a Demetra+ workspace file
#' (usually with extension `.xml`).
#'
#' @details
#' For each series in the workspace:
#' - the function looks up its assigned `regs` in the `cjo` table,
#' - translates the label (`REG1`, `REG2`, …) into the corresponding
#'   user-defined regressors (e.g. `r.REG1_Semaine`, `r.REG5_Lundi`, …),
#' - updates the `domainSpec` with `set_tradingdays(option = "UserDefined", ...)`,
#' - saves the modified workspace.
#'
#' The file is overwritten in place (`replace = TRUE`).
#'
#' @return
#' Invisibly returns `NULL`. The workspace file at `ws_path` is updated
#' on disk with the new CJO regressors.
#'
#' @examples
#' \dontrun{
#' # Load a workspace and apply INSEE CJO sets
#' cjo_table <- retrieve_cjo("workspace.xml")
#' affect_cjo(cjo = cjo_table, ws_path = "workspace.xml")
#' }
#'
#' @export
affect_cjo <- function(cjo, ws_path) {
    jws <- rjd3workspace::jws_open(file = ws_path)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    jsap <- rjd3workspace::jws_sap(jws, 1L)

    for (id_sai in seq_len(rjd3workspace::.jsap_sa_count(jsap))) {
        jsai <- rjd3workspace::.jsap_sa(jsap, idx = id_sai)
        series_name <- rjd3workspace::.jsa_name(jsai)
        cat(paste0(
            "Série ",
            series_name,
            ", ",
            id_sai,
            "/",
            rjd3workspace::.jsap_sa_count(jsap),
            "\n"
        ))

        # CJO
        jeu_regresseur <- cjo[which(cjo[["series"]] == series_name), "regs"]
        cjo_variables <- NULL
        if (any(grepl("REG1", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- "r.REG1_Semaine"
        } else if (any(grepl("REG2", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c("r.REG2_Semaine", "r.REG2_Samedi")
        } else if (any(grepl("REG3", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c(
                "r.REG3_Lundi",
                "r.REG3_Semaine",
                "r.REG3_Samedi"
            )
        } else if (any(grepl("REG5", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c(
                "r.REG5_Lundi",
                "r.REG5_Mardi",
                "r.REG5_Mercredi",
                "r.REG5_Jeudi",
                "r.REG5_Vendredi"
            )
        } else if (any(grepl("REG6", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c(
                "r.REG6_Lundi",
                "r.REG6_Mardi",
                "r.REG6_Mercredi",
                "r.REG6_Jeudi",
                "r.REG6_Vendredi",
                "r.REG6_Samedi"
            )
        }
        if (any(grepl("LY", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c(cjo_variables, "r.LY")
        }

        # Création de la spec
        sai <- read_sai(jsai)
        new_domainSpec <- domainSpec <- sai$domainSpec

        # Création de la spec
        new_domainSpec <- domainSpec |>
            set_tradingdays(
                option = "UserDefined",
                uservariable = cjo_variables,
                test = "None"
            )

        rjd3workspace::set_domain_specification(
            jsap = jsap,
            idx = id_sai,
            spec = new_domainSpec
        )
        rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
    }

    # Save WS automatique
    rjd3workspace::save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}
