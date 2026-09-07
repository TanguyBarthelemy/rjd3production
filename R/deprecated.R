#' @title Deprecated functions
#'
#' @param ws_path,threshold,reference,estimation,verbose Parameters.
#'
#' @returns
#' The same value as returned by the corresponding non-deprecated function.
#' The returned object represents an encoded identifier for a spreadsheet
#' series or collection.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#'
#' library("rjd3workspace")
#' library("rjd3x13")
#' library("rjd3toolkit")
#'
#' \donttest{
#' new_spec <- x13_spec() |>
#'     add_outlier(type = "LS", date = "1990-01-01")
#' jws <- create_ws_from_data(x = ABS[, 1, drop = FALSE], spec = new_spec)
#' path_ws <- tempfile(pattern = "ws", fileext = ".xml")
#' save_workspace(jws, file = path_ws)
#'
#' # `remove_non_significative_outliers` is deprecated.
#' # Use `remove_non_significant_outliers` instead
#'
#' # Remove non-significant outliers (p > 0.3) from a workspace
#' remove_non_significant_outliers(path_ws, threshold = 0.3, reference = TRUE)
#' }
#'
#' @name deprecated-rjd3production
NULL

#' @rdname deprecated-rjd3production
#' @export
remove_non_significative_outliers <- function(
    ws_path,
    threshold = 0.3,
    reference = FALSE,
    estimation = FALSE,
    verbose = TRUE
) {
    .Deprecated("remove_non_significant_outliers")
    remove_non_significant_outliers(
        ws_path,
        threshold,
        reference,
        estimation,
        verbose
    )
}
