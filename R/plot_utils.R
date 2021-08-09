#' Annotate publication date
#'
#' Add an annotation showing the date of the original scRNA-tools publication to
#' a plot with dates on the x-axis
#'
#' @return list of ggproto object
annotate_pub_date <- function() {

    list(
        ggplot2::geom_vline(
            xintercept = lubridate::ymd("2018-06-25"),
            size   = 1,
            colour = "#f781bf"
        ),
        ggplot2::annotate(
            "text",
            x          = lubridate::ymd("2018-06-06"),
            y          = Inf,
            label      = "scRNA-tools publication",
            size       = 5,
            angle      = 90,
            hjust      = 1.1,
            vjust      = -0.5,
            colour     = "#f781bf",
            family     = "Noto Sans"
        )
    )
}

#' Get date counts
#'
#' Get the total number of tools in the database at each date
#'
#' @param tools data.frame containing tools data
#'
#' @return data.frame with total number of tools per date
get_date_totals <- function(tools) {
    tools %>%
        dplyr::select(Date = Added) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        tidyr::complete(
            Date = tidyr::full_seq(Date, 1),
            fill = list(Count = 0)
        ) %>%
        dplyr::mutate(Total = cumsum(Count))
}

#' Add date grid lines
#'
#' Add grid lines to a plot where the x-axis shows date. Useful for when line
#' labels should appear to be outside the plotting area.
#'
#' @param plot_data data.frame containing the data to plot
#' @param col Name of the column containing dates for the x-axis
#' @param colour Colour for grid lines
#' @param minor Whether to plot minor grid lines
#'
#' @return list of ggplot2 geoms
add_date_gridlines <- function(plot_data, col = "Date", colour = "grey90",
                               minor = FALSE) {

    gridlines <- get_date_gridlines(plot_data, col)

    geoms <- ggplot2::geom_vline(
        xintercept = gridlines$major$Date,
        colour     = colour
    )

    if (minor) {
        geoms <- list(
            geoms,
            ggplot2::geom_vline(
                xintercept = gridlines$minor$Date,
                colour     = colour,
                size       = ggplot2::rel(0.5)
            )
        )
    }

    return(geoms)

}

#' Add y grid lines
#'
#' Add y-axis grid lines to a plot. Useful when grid lines should not span the
#' complete plotting area.
#'
#' @param ymin Minimum y value
#' @param ymax Maximum y value
#' @param ystep Space between y-axis grid lines
#' @param xmin Minium x value
#' @param xmax Maximum x value
#' @param colour Colour for grid lines
#'
#' @return ggplot2 geom
add_y_gridlines <- function(ymin, ymax, ystep, xmin, xmax, colour = "grey90") {

    gridlines <- tibble::tibble(
        xmin = xmin,
        xmax = xmax,
        y    = seq(ymin, ymax, ystep)
    )

    ggplot2::geom_segment(
        data = gridlines,
        ggplot2::aes(x = xmin, xend = xmax, y = y, yend = y),
        inherit.aes = FALSE,
        colour      = colour
    )

}

#' Get date grid lines
#'
#' Calculate positions for grid lines on a date axis
#'
#' @param plot_data data.frame containing data to plot
#' @param col Name of the column containing date data
#'
#' @return list of tibbles containing major and minor grid lines
get_date_gridlines <- function(plot_data, col = "Date") {

    min_date <- min(plot_data[[col]])
    max_date <- max(plot_data[[col]])

    min_year <- lubridate::floor_date(min_date, unit = "year")
    max_year <- lubridate::ceiling_date(max_date, unit = "year")

    major <- tibble::tibble(
        Date = c(
            min_date,
            seq(min_year, max_year, by = "year"),
            max_date
        )
    ) %>%
        dplyr::filter(dplyr::between(Date, min_date, max_date))

    minor <- tibble::tibble(
        Date = c(
            seq(min_year + 183, max_year, by = "year")
        )
    ) %>%
        dplyr::filter(dplyr::between(Date, min_date, max_date))

    list(major = major, minor = minor)
}
