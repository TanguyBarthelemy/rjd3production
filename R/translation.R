
spec1 <- rjd3x13::x13_spec()
spec2 <- spec1 |>
    rjd3toolkit::add_outlier(
        type = c("AO", "LS", "TC", "SO"),
        date = c("2020-01-01", "2024-01-01", "2018-10-01", "2010-02-01")
    ) |>
    rjd3toolkit::add_outlier(
        type = c("AO", "AO"),
        date = c("2021-11-01", "2022-03-01"),
        name = c("AO1TB", "AO2TB")
    ) |>
    rjd3toolkit::add_outlier(
        type = c("TC", "TC"),
        date = c("2012-01-01", "2024-12-01"),
        coef = c(0.58, 0.71)
    ) |>
    rjd3toolkit::add_outlier(
        type = c("LS", "LS"),
        date = c("2013-05-01", "2023-08-01"),
        coef = c(0.92, 0.23),
        name = c("LS1TB", "LS2TB")
    )

vect_to_string <- function(x) {
    if (is.character(x)) {

    }
}

rev_add_outliers <- function(x) {
    if (is.null(x$regarima$regression$outliers)) {
        return(NULL)
    }
    outliers <- x$regarima$regression$outliers
    out_types <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "code"
    ) |>
        deparse() |>
        paste0(collapse = "")
    out_dates <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "pos"
    ) |>
        deparse() |>
        paste0(collapse = "")
    out_names <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "name"
    ) |>
        deparse() |>
        paste0(collapse = "")
    out_coeff <- outliers |>
        lapply(FUN = "[[", "coef") |>
        lapply(FUN = "[[", "value") |>
        lapply(FUN = \(coeff) {
            if (is.null(coeff)) coeff <- 0L
            return(coeff)
        }) |>
        as.double() |>
        deparse() |>
        paste0(collapse = "")

    code <- paste(
        "add_outlier(x = rjd3x13::x13_spec(),\n",
        "type = ", out_types, ",\n",
        "date = ", out_dates, ",\n",
        "name = ", out_names, ",\n",
        "coef = ", out_coeff, "\n",
        ")"
    )
    return(code)
}

rev_add_outliers(spec2) |> cat()
spec3 <- eval(parse(text = rev_add_outliers(spec2)), envir = .GlobalEnv)
