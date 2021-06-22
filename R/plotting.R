#' Plot landscape UMAP
#'
#' Plot an overview UMAP of tools in the database
#'
#' @param mfa data.frame with MFA results
#' @param hcpc data.frame with HCPC results
#' @param umap data.frame with UMAP output
#'
#' @return ggplot object
plot_landscape_umap <- function(mfa, hcpc, umap) {
    plot_data <- mfa %>%
        dplyr::mutate(
            UMAP1   = umap[, 1],
            UMAP2   = umap[, 2],
            Cluster = hcpc$data.clust$clust
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$UMAP1, y = .data$UMAP2, colour = .data$Cluster)
    ) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
}

#' Plot tools over time
#'
#' Plot the number of tools in the database over time
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_tools_over_time <- function(tools) {
    plot_data <- tools %>%
        dplyr::select(Date = Added) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        tidyr::complete(
            Date = tidyr::full_seq(Date, 1),
            fill = list(Count = 0)
        ) %>%
        dplyr::mutate(Total = cumsum(Count))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Date, y = .data$Total)) +
        ggplot2::geom_vline(
            xintercept = lubridate::ymd("2018-06-25"),
            colour = "red"
        ) +
        ggplot2::annotate(
            "text",
            x = lubridate::ymd("2018-06-25"),
            y = Inf,
            label = "scRNA-tools publication",
            angle = 90,
            hjust = 1.1,
            vjust = -0.5,
            colour = "red"
        ) +
        ggplot2::geom_line(size = 1) +
        ggplot2::labs(
            y = "Number of tools in database"
        ) +
        ggplot2::theme_minimal()
}
