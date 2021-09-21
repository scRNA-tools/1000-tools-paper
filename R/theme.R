#' Theme 1000
#'
#' ggplot2 theme for the 1000 tools paper
#'
#' @param base_size Base font size (pts)
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size Base size for rect elements
#' @param plot_title_size Size for plot title text
#' @param border Whether to add a border around the plot area
#' @param border_col Colour for border
#' @param grid Whether to add a grid to the plot, either `"both"` (all grids),
#' `"none"` (no grids), `"x"` (x-axis only) or `"y"` (y-axis only)
#' @param grid_col Colour for grid lines
#'
#' @details
#' Inspired by `hbrthemes::theme_ipsum()`
#'
#' @return theme object
theme_1000 <- function(base_size = 11, base_family = "Noto Sans",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       plot_title_size = base_size * 1.6,
                       border = TRUE, border_col = "grey30",
                       grid = c("both", "none", "x", "y"),
                       grid_col = "grey90") {

    grid <- match.arg(grid)

    theme <- ggplot2::theme_minimal(
        base_size, base_family, base_line_size, base_rect_size
    )

    if (border) {
        theme <- theme + ggplot2::theme(
            panel.border = ggplot2::element_rect(
                colour = border_col,
                fill   = "NA"
            )
        )
    }

    if (grid == "none") {
        theme <- theme + ggplot2::theme(panel.grid = ggplot2::element_blank())
    } else {

        theme <- theme + ggplot2::theme(
            panel.grid = ggplot2::element_line(
                colour = grid_col,
                size   = 0.3
            ),
            panel.grid.major = ggplot2::element_line(
                colour = grid_col,
                size   = 0.3
            ),
            panel.grid.minor = ggplot2::element_line(
                colour = grid_col,
                size   = ggplot2::rel(0.5)
            )
        )

        if (grid == "x") {
            theme <- theme + ggplot2::theme(
                panel.grid.major.y = ggplot2::element_blank(),
                panel.grid.minor.y = ggplot2::element_blank()
            )
        }

        if (grid == "y") {
            theme <- theme + ggplot2::theme(
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.x = ggplot2::element_blank()
            )
        }
    }

    theme + ggplot2::theme(
        plot.title = ggplot2::element_text(
            hjust  = 0,
            size   = plot_title_size,
            margin = ggplot2::margin(b = plot_title_size * 0.3),
            family = base_family,
            face   = "bold"
        ),
        axis.title.x = ggplot2::element_text(
            hjust  = 0,
            size   = base_size,
            family = base_family,
            face   = "bold"
        ),
        axis.title.y = ggplot2::element_text(
            hjust  = 0,
            size   = base_size,
            family = base_family,
            face   = "bold"
        ),
        legend.title = ggplot2::element_text(
            size   = base_size,
            family = base_family,
            face   = "bold",
            margin = ggplot2::margin(r = 10)
        )
    )
}

#' Theme 1000 bar
#'
#' ggplot2 theme for bar charts in the the 1000 tools paper
#'
#' @param direction Direction of bar plot
#' @param line_colour Colour for base line
#' @param ... Arguments passed to `theme_1000()`
#'
#' @return theme object
theme_1000_bar <- function(direction = c("v", "h"), line_colour = "grey30",
                           ...) {

    direction <- match.arg(direction)

    theme <- theme_1000(border = FALSE, grid = "none", ...) +
        ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text    = ggplot2::element_blank(),
            axis.ticks   = ggplot2::element_blank()
        )

    switch (direction,
        v = theme + ggplot2::theme(
            axis.line.x = ggplot2::element_line(colour = line_colour)
        ),
        h = theme + ggplot2::theme(
            axis.line.y = ggplot2::element_line(colour = line_colour)
        )
    )

}

#' Bar scales
#'
#' Standard scales for bar plots
#'
#' @param direction Direction of bar plot
#' @param expansion_mult Expansion multiplier for continuous axis
#'
#' @return list of ggproto scale objects
bar_scales <- function(direction = c("v", "h"), expansion_mult = 0.12) {

    direction <- match.arg(direction)

    switch (direction,
        v = list(
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.5)),
            ggplot2::scale_y_continuous(
                expand = ggplot2::expansion(mult = c(0, expansion_mult))
            ),
            ggplot2::scale_fill_brewer(palette = "Set1")
        ),
        h = list(
            ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.5)),
            ggplot2::scale_x_continuous(
                expand = ggplot2::expansion(mult = c(0, expansion_mult))
            ),
            ggplot2::scale_fill_brewer(palette = "Set1")
        )
    )
}
