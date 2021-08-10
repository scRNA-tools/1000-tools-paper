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

#' Plot category proportions
#'
#' Plot the proportion of tools in each category. Lines show the whole database
#' and points show tools added in each year.
#'
#' @param categories data.frame containing categories data
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_category_props <- function(categories, tools) {

    cats_summ <- categories %>%
        dplyr::summarise(
            dplyr::across(dplyr::starts_with("Cat"), sum, na.rm = TRUE)
        ) %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Cat"),
            names_to  = "Category",
            values_to = "Count"
        ) %>%
        dplyr::mutate(Category = stringr::str_remove(Category, "Cat")) %>%
        dplyr::mutate(Prop = Count / nrow(tools)) %>%
        dplyr::arrange(Prop) %>%
        dplyr::mutate(Category = factor(Category, levels = unique(Category)))

    cats_by_year <- tools %>%
        dplyr::mutate(YearAdded = lubridate::year(Added)) %>%
        dplyr::left_join(categories, by = "Tool") %>%
        dplyr::select(Tool, YearAdded, dplyr::starts_with("Cat")) %>%
        dplyr::group_by(YearAdded) %>%
        dplyr::summarise(
            Tools = dplyr::n(),
            dplyr::across(dplyr::starts_with("Cat"), sum, na.rm = TRUE)
        ) %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Cat"),
            names_to  = "Category",
            values_to = "YearCount"
        ) %>%
        dplyr::mutate(Category = stringr::str_remove(Category, "Cat")) %>%
        dplyr::mutate(YearProp = YearCount / Tools) %>%
        dplyr::mutate(
            Category = factor(Category, levels = levels(cats_summ$Category))
        )

    ggplot2::ggplot(cats_summ, ggplot2::aes(y = Category)) +
        ggplot2::geom_point(
            ggplot2::aes(x = Prop),
            colour = "red",
            shape  = "|",
            size   = 5
        ) +
        ggplot2::geom_point(
            data = cats_by_year,
            ggplot2::aes(
                x = YearProp,
                fill = forcats::fct_rev(factor(YearAdded))
            ),
            shape = 21,
            size  = 2
        ) +
        ggplot2::scale_fill_brewer(palette = "RdPu", name = "Year added") +
        ggplot2::theme_minimal()
}

#' Plot category barcodes
#'
#' Plot a barcode plot showing when tools where added to each category
#'
#' @param categories data.frame containing categories data
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_category_barcodes <- function(categories, tools) {

    cats_summ <- categories %>%
        dplyr::summarise(
            dplyr::across(dplyr::starts_with("Cat"), sum, na.rm = TRUE)
        ) %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Cat"),
            names_to  = "Category",
            values_to = "Count"
        ) %>%
        dplyr::mutate(Category = stringr::str_remove(Category, "Cat")) %>%
        dplyr::mutate(Prop = Count / nrow(tools)) %>%
        dplyr::arrange(Prop) %>%
        dplyr::mutate(Category = factor(Category, levels = unique(Category)))

    cats_dates <- tools %>%
        dplyr::left_join(categories, by = "Tool") %>%
        dplyr::select(Added, dplyr::starts_with("Cat")) %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Cat"),
            names_to     = "Category",
            values_to    = "Present",
            names_prefix = "Cat"
        ) %>%
        dplyr::filter(Present) %>%
        dplyr::mutate(
            Category = factor(Category, levels = levels(cats_summ$Category))
        )

    ggplot2::ggplot(
        cats_dates,
        ggplot2::aes(x = Added, y = Category, colour = Category)
    ) +
        ggplot2::geom_point(alpha = 0.5, shape = "|", size = 5.5) +
        # ggbeeswarm::geom_quasirandom(groupOnX = FALSE, alpha = 0.3) +
        ggplot2::scale_colour_hue(l = 50) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
}

#' Plot users
#'
#' Plot number of scRNA-tools.org users over time
#'
#' @param ga_users data.frame containing users data
#'
#' @return ggplot object
plot_users <- function(ga_users) {
    plot_data <- ga_users %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Users"),
            names_to = "Type",
            values_to = "Users",
            names_prefix = "Users"
        ) %>%
        dplyr::mutate(
            Type = factor(
                Type,
                levels = c("Day", "Week", "Month")
            )
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Date, y = Users, colour = Type, fill = Type)
    ) +
        annotate_pub_date() +
        ggplot2::geom_point(size = 0.3, alpha = 0.5) +
        ggplot2::geom_smooth(method = "loess", formula = "y ~ x") +
        ggplot2::scale_x_date(
            breaks = scales::pretty_breaks(n = 10),
            date_labels = "%b %Y"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank()
        )
}

#' Plot users map
#'
#' Plot a map showing which countries scRNA-tools.org users come from
#'
#' @param ga_countries data.frame containing countries data
#'
#' @return ggplot object
plot_users_map <- function(ga_countries) {
    world <- ggplot2::map_data("world")

    china <- world %>%
        dplyr::filter(region == "China") %>%
        dplyr::mutate(
            region = dplyr::case_when(
                subregion == "Hong Kong" ~ "Hong Kong",
                subregion == "Macao"     ~ "Macao",
                TRUE                     ~ region
            )
        )

    world <- world %>%
        dplyr::filter(region != "China") %>%
        dplyr::bind_rows(china)

    ga_countries <- ga_countries %>%
        dplyr::filter(Country != "(not set)")

    if (!all(ga_countries$Country %in% world$region)) {
        stop("Some countries not matched")
    }

    plot_data <- dplyr::left_join(
        world,
        ga_countries,
        by = c(region = "Country")
    )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = long, y = lat, group = group, fill = log10(Prop))
    ) +
        ggplot2::geom_polygon() +
        ggplot2::scale_fill_viridis_c(na.value = "grey80") +
        ggplot2::coord_fixed(expand = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title        = ggplot2::element_blank(),
            axis.text         = ggplot2::element_blank(),
            axis.ticks        = ggplot2::element_blank(),
            axis.ticks.length = grid::unit(0, "pt"),
            panel.grid        = ggplot2::element_blank(),
            legend.position   = "none",
            legend.key.width  = grid::unit(1, "cm"),
            plot.margin       = grid::unit(c(0, 0, 0, 0), "mm")
        )
}

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
        dplyr::arrange(Package)
    edges <- dplyr::bind_rows(r_dependencies, pypi_dependencies)

    graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges) %>%
        tidygraph::activate(nodes) %>%
        tidygraph::mutate(
            Degree = tidygraph::centrality_degree(mode = "total"),
            Centrality = tidygraph::centrality_pagerank()
        ) %>%
        tidygraph::mutate(IsCentral = Centrality > 0.005)

    set.seed(1)
    ggraph::ggraph(graph, layout = "fr") +
        ggraph::geom_edge_fan(
            ggplot2::aes(colour = Type),
            width   = 0.2,
            arrow   = grid::arrow(length = grid::unit(2, "mm")),
            end_cap = ggraph::circle(1, "mm")
        ) +
        ggraph::geom_node_point(ggplot2::aes(colour = Repo, size = Degree)) +
        ggraph::geom_node_label(
            ggplot2::aes(label = Package, colour = Repo, alpha = IsCentral),
            repel = TRUE,
            size = 2,
            box.padding = 0.1,
            label.padding = 0.1,
            max.overlaps = 100
        ) +
        ggplot2::scale_alpha_manual(values = c(0, 1)) +
        ggraph::scale_edge_color_brewer(palette = "Dark2") +
        ggraph::theme_graph()
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
