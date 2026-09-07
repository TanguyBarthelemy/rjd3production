#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
    options(
        rjd3production.thresholds = list(
            ly_signif = c(Signif = 0.1, Non_Signif = 1.0),
            res_td = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = 1.0),
            grade = c(Good = 0L, Uncertain = 1L, Bad = 3L, Severe = 5L),
            weights = c(res_td_sa_all = 2L, res_td_i_all = 1L)
        )
    )
}
