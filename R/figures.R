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

    # Required because of some interaction between {ggtext} and {cowplot}
    extrafont::loadfonts(quiet = TRUE)

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
        labels = c("A", "B"),
        label_size = 10
    )

    left <- cowplot::plot_grid(
        top,
        bars,
        nrow = 2,
        rel_heights = c(1, 1.1),
        labels = c("", "C"),
        label_size = 10,
        vjust  = 1
    )

    cowplot::plot_grid(
        left,
        categories_bar_plot,
        nrow       = 1,
        rel_widths = c(1, 0.4),
        labels     = c("", "D"),
        label_size = 10,
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
#' @param wordclouds_plot ggplot object with abstract wordclouds plot
#'
#' @return assembled ggplot object
make_trends_figure <- function(platforms_time_plot, category_trend_plot,
                               word_trends_plot, wordclouds_plot) {

    extrafont::loadfonts(quiet = TRUE)

    cowplot::plot_grid(
        cowplot::plot_grid(
            platforms_time_plot,
            category_trend_plot,
            nrow       = 1,
            rel_widths = c(1, 1),
            labels     = c("A", "B"),
            label_size = 10
        ),
        word_trends_plot,
        wordclouds_plot,
        nrow        = 3,
        rel_heights = c(1, 1, 0.5),
        labels      = c("", "C", "D"),
        label_size  = 10
    )
}

#' Make open science figure
#'
#' Assemble a figure showing the effect of open science on scRNA-seq analysis
#' tools
#'
#' @param linked_prop_bar ggplot with linked publications proportion bar plot
#' @param delay_plot ggplot object with delay in publication plot
#' @param gh_stats_plot ggplot object with GitHub stats overview plot
#' @param publications_models_plot ggplot object with publications models plot
#' @param tools_models_plot ggplot object with tools models plot
#'
#' @return assembled ggplot object
make_open_figure <- function(linked_prop_bar, delay_plot, gh_stats_plot,
                             publications_models_plot, tools_models_plot) {

    extrafont::loadfonts(quiet = TRUE)
    cowplot::set_null_device("agg")

    cowplot::plot_grid(
        cowplot::plot_grid(
            gh_stats_plot,
            linked_prop_bar,
            delay_plot,
            nrow       = 1,
            rel_widths = c(0.45, 0.2, 1),
            labels     = c("A", "B", "C"),
            label_size = 10
        ),
        cowplot::plot_grid(
            publications_models_plot,
            tools_models_plot,
            nrow   = 1,
            labels = c("D", "E"),
            label_size = 10
        ),
        nrow = 2,
        rel_heights = c(1, 1.2)
    )
}
