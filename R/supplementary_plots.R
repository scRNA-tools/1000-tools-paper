#' Plot dependencies
#'
#' Plot a graph showing dependencies between packages
#'
#' @param r_dependencies data.frame with dependencies between R packages
#' @param pypi_dependencies data.frame with dependencies between PyPI packages
#'
#' @return ggplot2 object
plot_dependencies <- function(r_dependencies, pypi_dependencies) {

    bioc_pkgs <- BiocPkgTools::biocPkgList()

    r_nodes <- tibble::tibble(
        Package = sort(unique(c(r_dependencies$From, r_dependencies$To)))
    ) %>%
        dplyr::mutate(
            Repo = dplyr::if_else(
                Package %in% bioc_pkgs$Package,
                "Bioconductor",
                "CRAN"
            )
        )

    py_nodes <- tibble::tibble(
        Package = sort(unique(c(pypi_dependencies$From, pypi_dependencies$To)))
    ) %>%
        dplyr::mutate(Repo = "PyPI")

    nodes <- dplyr::bind_rows(r_nodes, py_nodes) %>%
        dplyr::arrange(Package) %>%
        dplyr::mutate(
            Repo = factor(Repo)
        )
    edges <- dplyr::bind_rows(r_dependencies, pypi_dependencies) %>%
        dplyr::mutate(
            Type = factor(
                Type,
                levels = c(
                    "Depends", "Imports", "Suggests", "Requires", "Extra"
                )
            )
        )

    graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges) %>%
        tidygraph::activate(nodes) %>%
        tidygraph::mutate(
            Degree     = tidygraph::centrality_degree(mode = "total"),
            Centrality = tidygraph::centrality_pagerank()
        )

    r_plot <- graph %>%
        tidygraph::filter(Repo != "PyPI") %>%
        plot_dependencies_graph() +
        ggplot2::labs(title = "R packages")

    py_plot <- graph %>%
        tidygraph::filter(Repo == "PyPI") %>%
        plot_dependencies_graph() +
        ggplot2::labs(title = "Python packages")

    patchwork::wrap_plots(
        r_plot,
        py_plot
    ) +
        patchwork::plot_layout(
            ncol   = 1,
            guides = "collect"
        )
}

#' Plot dependencies graph
#'
#' Plot a graph object showing dependencies between packages
#'
#' @param graph tbl_graph object containing dependencies
#'
#' @return ggplot2 object
plot_dependencies_graph <- function(graph) {

    graph <- graph %>%
        tidygraph::mutate(IsCentral = rank(-Centrality) <= 25)

    withr::with_seed(1, {
        ggraph::ggraph(graph, layout = "fr") +
            ggraph::geom_edge_fan(
                ggplot2::aes(colour = Type),
                width   = 0.2,
                arrow   = grid::arrow(length = grid::unit(2, "mm")),
                end_cap = ggraph::circle(2, "mm")
            ) +
            ggraph::geom_node_point(
                ggplot2::aes(colour = Repo, size = Degree),
                shape = 21,
                fill = "white"
            ) +
            ggraph::geom_node_label(
                ggplot2::aes(
                    filter = IsCentral, label = Package, colour = Repo
                ),
                repel             = TRUE,
                size              = 2.5,
                label.padding     = 0.1,
                segment.size      = 0.7,
                segment.alpha     = 0.5,
                segment.linetype  = "dotted",
                box.padding       = 0.2,
                seed              = 1
            ) +
            ggplot2::scale_size(limits = c(0, 100)) +
            ggplot2::scale_colour_brewer(
                palette = "Set1",
                drop    = FALSE,
                name    = "Repository"
            ) +
            ggraph::scale_edge_color_brewer(
                palette = "Dark2",
                drop = FALSE
            ) +
            ggraph::theme_graph(base_family = "Noto Sans")
    })
}

#' Plot categories platforms
#'
#' Plot platform proportions for each analysis category
#'
#' @param categories_idx data.frame containing categories index
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_categories_platforms <- function(categories_idx, tools) {

    plot_data <- categories_idx %>%
        dplyr::left_join(tools, by = "Tool") %>%
        dplyr::mutate(
            RPython = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            )
        ) %>%
        dplyr::select(Tool, RPython, Category) %>%
        dplyr::group_by(Category, RPython) %>%
        dplyr::count(name = "Count") %>%
        dplyr::group_by(Category) %>%
        dplyr::mutate(Total = sum(Count)) %>%
        dplyr::mutate(Prop = Count / sum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Total) %>%
        dplyr::mutate(
            Category = stringr::str_replace_all(
                Category, "([[:upper:]])", " \\1"
            ),
            Category = stringr::str_trim(Category),
            Category = stringr::str_to_sentence(Category),
            Category = dplyr::case_when(
                Category == "U m is"                   ~ "UMIs",
                Category == "Dimensionality reduction" ~ "Dim. red.",
                Category == "Differential expression"  ~ "Diff. expression",
                TRUE                                   ~ Category
            ),
            Category = factor(Category, levels = unique(Category)),
            RPython = factor(
                RPython,
                levels = c("R", "Both", "Python", "Other")
            )
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Prop, y = Category, fill = RPython)
    ) +
        ggplot2::geom_col() +
        ggplot2::geom_vline(xintercept = c(0.25, 0.5, 0.75), colour = "white") +
        bar_scales(direction = "h") +
        ggplot2::scale_x_continuous(
            labels = scales::percent,
            expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(fill = "R/Python") +
        theme_1000_bar(direction = "h", base_size = 16) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(),
            axis.text.y = ggplot2::element_text(),
            legend.position = "bottom"
        )
}
