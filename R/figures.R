#' Make overview figure
#'
#' Assemble a figure showing an overview of the scRNA-tools database
#'
#' @param platforms_bar_plot ggplot object with platforms bar chart
#' @param repositories_bar_plot ggplot object with repositories bar chart
#' @param licenses_bar_plot ggplot object with licenses bar chart
#' @param tools_plot ggplot object with number of tools over time plot
#' @param pub_status_plot ggplot object with publication status bar chart
#' @param categories_bar_plot ggplot object with categories bar chart
#'
#' @return assembled ggplot object
make_overview_figure <- function(platforms_bar_plot, repositories_bar_plot,
                                 licenses_bar_plot, tools_plot,
                                 pub_status_plot, categories_bar_plot) {

    bars <- patchwork::wrap_plots(
        platforms_bar_plot,
        repositories_bar_plot,
        licenses_bar_plot,
        nrow   = 1,
        guides = "collect"
    ) &
        ggplot2::theme(legend.position = "bottom")

    top <- cowplot::plot_grid(
        tools_plot,
        pub_status_plot,
        nrow   = 1,
        labels = c("A", "B")
    )

    left <- cowplot::plot_grid(
        top,
        bars,
        nrow = 2,
        rel_heights = c(1, 1.1),
        labels = c("", "D"),
        vjust  = 1
    )

    cowplot::plot_grid(
        left,
        categories_bar_plot,
        nrow       = 1,
        rel_widths = c(1, 0.3),
        labels     = c("", "C"),
        hjust      = 1
    )
}

#' Make trends figure
#'
#' Assemble a figure showing an overview of trends in scRNA-seq analysis tools
#'
#' @param platforms_time_plot ggplot object with platforms over time plot
#' @param category_trend_plot ggplot object with categories trends plot
#' @param word_trends_plot ggplot object with abstract words trends plot
#'
#' @return assembled ggplot object
make_trends_figure <- function(platforms_time_plot, category_trend_plot,
                               word_trends_plot) {
    patchwork::wrap_plots(
        A = platforms_time_plot,
        B = category_trend_plot,
        C = word_trends_plot,
        design = "
            AB
            CC
        ",
        heights = c(1, 0.8)
    ) +
        patchwork::plot_annotation(tag_levels = "A")
}

#' Make open science figure
#'
#' Assemble a figure showing the effect of open science on scRNA-seq analysis
#' tools
#'
#' @param delay_plot ggplot object with delay in publication plot
#' @param gh_stats_plot ggplot object with GitHub stats overview plot
#' @param publications_models_plot ggplot object with publications models plot
#' @param tools_models_plot ggplot object with tools models plot
#'
#' @return assembled ggplot object
make_open_figure <- function(delay_plot, gh_stats_plot,
                             publications_models_plot, tools_models_plot) {
    cowplot::plot_grid(
        cowplot::plot_grid(
            delay_plot,
            gh_stats_plot,
            nrow       = 1,
            rel_widths = c(1, 0.6),
            labels     = c("A", "B")
        ),
        cowplot::plot_grid(
            publications_models_plot,
            tools_models_plot,
            nrow   = 1,
            labels = c("C", "D")
        ),
        nrow = 2
    )
}
