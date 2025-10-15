#' @title Extract all series from a SAI
#'
#' @description
#' Extracts all available time series (pre-adjustment, decomposition, and final)
#' from a seasonal adjustment item (`jsai`) inside a Demetra+ workspace.
#'
#' @param jsai A Java Seasonal Adjustment Item object, typically obtained via
#'   [jsap_sai()] after opening and computing a workspace with [jws_open()]
#'   and [jws_compute()].
#'
#' @return A `data.frame` with columns:
#' - `SAI`: name of the SAI,
#' - `series`: the type of series (e.g. `"y"`, `"sa"`, `"trend"`),
#' - `date`: observation dates,
#' - `value`: numeric values of the series.
#'
#' @examples
#' \dontrun{
#' path <- file.path(tempdir(), "workspace_RSA3.xml")
#' jws <- jws_open(path)
#' jws_compute(jws)
#' jsap <- jws_sap(jws, 1L)
#' jsai <- jsap_sai(jsap, 1L)
#'
#' df <- get_series(jsai)
#' head(df)
#' }
#'
#' @export
get_series <- function(jsai) {
    res <- read_sai(jsai)$results
    if (is.null(res)) {
        stop("Please compute your WS.")
        return(invisible(NULL))
    }
    output <- NULL
    all_series <- c(res$preadjust, res$decomposition, res$final)
    for (s in names(all_series)) {
        series <- all_series[[s]]
        if (!is.null(series)) {
            output <- rbind(
                output,
                data.frame(
                    series = s,
                    date = series |> time() |> zoo::as.Date(),
                    value = as.numeric(series)
                )
            )
        }
    }
    return(cbind(SAI = sai_name(jsai), output))
}

#' @title Retrieve a SAI by its name
#'
#' @description
#' Searches a workspace for a seasonal adjustment item (SAI) whose name matches
#' the user-supplied string and returns the corresponding object.
#'
#' @param jws A Java Workspace object, as returned by [jws_open()].
#' @param series_name [character] Name of the SAI to retrieve.
#'
#' @return A Java Seasonal Adjustment Item object (`jsai`).
#'
#' @examples
#' \dontrun{
#' path <- file.path(tempdir(), "workspace_RSA3.xml")
#' jws <- jws_open(path)
#' jws_compute(jws)
#'
#' jsai <- get_jsai_by_name(jws, "series_1")
#' df <- get_series(jsai)
#' head(df)
#' }
#'
#' @export
get_jsai_by_name <- function(jws, series_name) {
    jsap <- jws_sap(jws, idx = 1L)
    sai_names <- sap_sai_names(jsap)
    id <- which(sai_names == series_name)
    if (length(id) == 0) {
        stop("No SAI are named after ", series_name)
    }
    if (length(id) > 1) {
        stop("More than one SAI is named after ", series_name)
    }
    return(jsap_sai(jsap, idx = id))
}

#' @title Compare series across workspaces
#'
#' @description
#' Reads multiple Demetra+ workspaces and extracts comparable series
#' (by SAI and series type), returning them in a tidy format.
#' This is particularly useful to compare results across different
#' specifications (e.g. RSA3 vs RSA5).
#'
#' @param ws_paths [character] Vector of workspace file paths.
#' @param series_names [character] Vector of SAI names to compare.
#'
#' @return A `data.frame` with columns:
#' - `ws`: workspace name (derived from file basename),
#' - `SAI`: SAI name,
#' - `series`: type of series,
#' - `date`: observation date,
#' - `value`: numeric value.
#'
#' @examples
#' \dontrun{
#' # Two demo workspaces (RSA3 and RSA5)
#' path1 <- file.path(tempdir(), "workspace_RSA3.xml")
#' path2 <- file.path(tempdir(), "workspace_RSA5.xml")
#'
#' df <- compare(c(path1, path2), series_names = "series_1")
#' head(df)
#'
#' # In a Shiny app, you can filter and plot:
#' library(ggplot2)
#' ggplot(df, aes(x = date, y = value, color = ws, linetype = series)) +
#'   geom_line() +
#'   geom_point()
#' }
#'
#' @export
compare <- function(ws_paths, series_names) {
    ws_paths <- normalizePath(ws_paths)

    if (missing(series_names)) {
        series_names <- ws_paths[1L] |>
            rjd3workspace::jws_open() |>
            rjd3workspace::jws_sap(idx = 1L) |>
            rjd3workspace::sap_sai_names()
    }

    output <- NULL
    for (ws_path in ws_paths) {
        jws <- jws_open(ws_path)
        ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
        jws_compute(jws)
        for (series_name in series_names) {
            series <- get_jsai_by_name(jws = jws, series_name = series_name) |>
                get_series()
            output <- rbind(output, cbind(ws = ws_name, series))
        }
    }
    return(output)
}

#' @title Run the Shiny comparison app
#'
#' @description
#' Launches an interactive Shiny application to explore and compare
#' seasonal adjustment results stored in a `data.frame` returned by [compare()].
#'
#' @param data A `data.frame` returned by [compare()], containing
#'   the columns `ws`, `SAI`, `series`, `date`, and `value`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return Runs a Shiny app in the R session (no return value).
#'
#' @examples
#' \dontrun{
#' df <- compare(c(path1, path2), series_names = "series_1")
#' run_app(df)
#' }
#'
#' @export
run_app <- function(data, ...) {
    stopifnot(all(c("ws", "SAI", "series", "date", "value") %in% names(data)))

    ui <- shiny::fluidPage(
        shiny::titlePanel("Comparateur de séries"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::selectInput("sai", "Choisir un SAI :",
                                   choices = unique(data$SAI),
                                   selected = unique(data$SAI)[1]),
                shiny::selectInput("serie", "Choisir une série :",
                                   choices = unique(data$series),
                                   selected = unique(data$series)[1]),
                shiny::checkboxInput("filter_by_sai", "Filtrer par SAI", value = TRUE),
                shiny::checkboxInput("filter_by_serie", "Filtrer par série", value = FALSE)
            ),
            shiny::mainPanel(
                shiny::plotOutput("plot")
            )
        )
    )

    server <- function(input, output, session) {
        filtered_data <- shiny::reactive({
            d <- data
            if (input$filter_by_sai) d <- d[d$SAI == input$sai, ]
            if (input$filter_by_serie) d <- d[d$series == input$serie, ]
            d
        })

        output$plot <- shiny::renderPlot({
            ggplot2::ggplot(filtered_data(),
                            ggplot2::aes(x = date, y = value, color = ws, linetype = series)) +
                ggplot2::geom_line(size = 1) +
                ggplot2::geom_point() +
                ggplot2::labs(
                    title = "Comparaison des séries",
                    subtitle = paste("SAI:", input$sai, "| Série:", input$serie),
                    x = "Date", y = "Valeur"
                ) +
                ggplot2::theme_minimal()
        })
    }

    shiny::shinyApp(ui, server, ...)
}
