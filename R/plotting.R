plot_landscape_umap <- function(data, hcpc, umap) {
    data %>%
        dplyr::mutate(
            UMAP1   = umap[, 1],
            UMAP2   = umap[, 2],
            Cluster = hcpc$data.clust$clust
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = UMAP1, y = UMAP2, colour = Cluster)) +
            ggplot2::geom_point() +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = "none")
}
