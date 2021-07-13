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

    ggplot2::ggplot(
        get_date_totals(tools),
        ggplot2::aes(x = .data$Date, y = .data$Total)
    ) +
        annotate_pub_date() +
        ggplot2::geom_line(size = 1) +
        ggplot2::labs(
            y = "Number of tools in database"
        ) +
        ggplot2::theme_minimal()
}

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
            colour = "red"
        ),
        ggplot2::annotate(
            "text",
            x = lubridate::ymd("2018-06-06"),
            y = Inf,
            label = "scRNA-tools publication",
            angle = 90,
            hjust = 1.1,
            vjust = -0.5,
            colour = "red"
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

#' Plot platforms over time
#'
#' Plot the proportion of tools for each platform over time
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_platforms_over_time <- function(tools) {

    platform_dates <- tools %>%
        dplyr::select(
            Date = Added,
            dplyr::starts_with("Platform"),
            -Platform
        ) %>%
        dplyr::mutate(
            PlatformOther = !(PlatformR | PlatformPy | PlatformCPP |
                                  PlatformMATLAB)
        ) %>%
        tidyr::pivot_longer(
            dplyr::starts_with("Platform"),
            names_to     = "Platform",
            names_prefix = "Platform",
            values_to    = "Present"
        ) %>%
        dplyr::group_by(Date, Platform) %>%
        dplyr::summarise(Count = sum(Present), .groups = "drop") %>%
        dplyr::group_by(Date) %>%
        tidyr::complete(
            Date = tidyr::full_seq(Date, 1),
            Platform,
            fill = list(Count = 0)
        ) %>%
        dplyr::arrange(Platform, Date) %>%
        dplyr::group_by(Platform) %>%
        dplyr::mutate(PlatformTotal = cumsum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(get_date_totals(tools), by = "Date") %>%
        dplyr::mutate(Prop = PlatformTotal / Total) %>%
        dplyr::mutate(
            Platform = factor(
                Platform,
                levels = c("R", "Py", "CPP", "MATLAB", "Other"),
                labels = c("R", "Python", "C++", "MATLAB", "Other")
            )
        )

    ggplot2::ggplot(
        platform_dates,
        ggplot2::aes(x = Date, y = Prop, colour = Platform)
    ) +
        annotate_pub_date() +
        ggplot2::geom_line(size = 1) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::geom_text(
            data = dplyr::filter(platform_dates, Date == dplyr::last(Date)),
            ggplot2::aes(label = Platform),
            hjust = -0.1
        ) +
        ggplot2::scale_x_date(
            expand = ggplot2::expansion(mult = c(0.01, 0.1))
        ) +
        ggplot2::labs(
            x = "Date",
            y = "Percentage of tools in database"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "none"
        )
}

#' Plot publication delay
#'
#' Create a raincloud plot showing the number of days between preprints and
#' linked publications
#'
#' @param ref_links data.frame containing reference links
#' @param references data.frame containing references data
#'
#' @return ggplot object
plot_publication_delay <- function(ref_links, references) {

    delays <- ref_links %>%
        dplyr::filter(Correct) %>%
        dplyr::left_join(references, by = c(Preprint = "DOI")) %>%
        dplyr::select(Preprint, Publication, PreprintDate = Date) %>%
        dplyr::left_join(
            references,
            by = c(Publication = "DOI"),
            suffix = c("", "Is")
        ) %>%
        dplyr::select(
            Preprint, Publication, PreprintDate, PublicationDate = Date
        ) %>%
        dplyr::mutate(
            Delay = as.numeric(PublicationDate - PreprintDate, units = "days")
        ) %>%
        dplyr::filter(Delay > 0)

    ggplot2::ggplot(delays, ggplot2::aes(x = Delay)) +
        ggdist::stat_halfeye(
            adjust        = 1,
            width         = 0.6,
            justification = -0.1,
            .width        = 0,
            point_colour  = NA
        ) +
        ggplot2::geom_boxplot(
            width         = 0.1,
            outlier.shape = NA
        ) +
        ggdist::geom_dots(
            # ggplot2::aes(
            #     fill   = factor(lubridate::year(Publication)),
            #     group = NA
            # ),
            side          = "bottom",
            justification = 1.1,
            binwidth      = 10,
            layout        = "weave",
            stackratio    = 1.1
        ) +
        ggplot2::scale_x_continuous(breaks = seq(0, 1500, 100)) +
        ggplot2::coord_cartesian(ylim = c(-0.5, NA)) +
        ggplot2::labs(
            x = "Days between preprint and publication"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title.y       = ggplot2::element_blank(),
            axis.text.y        = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
        )
}

#' Plot publication status
#'
#' Create a bar plot of the current publication status
#'
#' @param tools data.frame containing tools information
#'
#' @return ggplot2 object
plot_publication_status <- function(tools) {
    plot_data <- tools %>%
        dplyr::mutate(
            IsPublished = Publications > 0,
            IsPreprint  = Preprints > 0
        ) %>%
        dplyr::mutate(
            PubStatus = dplyr::case_when(
                IsPublished ~ "Published",
                IsPreprint  ~ "Preprint",
                TRUE        ~ "Unpublished"
            )
        ) %>%
        dplyr::mutate(
            PubStatus = factor(
                PubStatus,
                levels = c("Published", "Preprint", "Unpublished")
            )
        ) %>%
        dplyr::group_by(PubStatus) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::mutate(Prop = Count / sum(Count)) %>%
        dplyr::mutate(
            PctStr = format(
                Prop * 100,
                digits = 1,
                nsmall = 1
            ),
            Label = glue::glue("{PubStatus}\n({Count}, {PctStr}%)")
        )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = PubStatus, y = Count)) +
        ggplot2::geom_col(ggplot2::aes(fill = PubStatus)) +
        ggplot2::geom_text(
            ggplot2::aes(label = Label),
            vjust  = 1.5,
            size   = 5,
            colour = "white"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "none",
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}

#' Plot GitHub stats
#'
#' Create a text plot of some GitHub summary statistics
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_gh_stats <- function(tools) {

    stats <- tibble::tribble(
                ~ Stat,                                      ~ Value,
               "Repos",                            sum(tools$GitHub),
              "Owners",                length(unique(tools$GHOwner)),
        "Contributors",      sum(tools$GHContributors, na.rm = TRUE),
             "Commits",           sum(tools$GHCommits, na.rm = TRUE),
               "Hours", sum(tools$GHCommits, na.rm = TRUE) * 10 / 60
    ) %>%
        dplyr::mutate(Stat = factor(Stat, levels = Stat)) %>%
        dplyr::mutate(
            ValueLabel = dplyr::if_else(
                Stat == "Hours",
                paste0("~", signif(Value, 2)),
                as.character(Value)
            )
        )

    ggplot2::ggplot(stats, ggplot2::aes(x = "A", y = forcats::fct_rev(Stat))) +
        ggplot2::geom_text(
            ggplot2::aes(label = ValueLabel),
            size    = 20,
            colour  = "dodgerblue",
            hjust   = 1,
            nudge_x = -0.01
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = Stat),
            size    = 20,
            colour  = "black",
            hjust   = 0,
            nudge_x = 0.01
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
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

#' Plot category proportion trend
#'
#' Plot a scatter plot showing the trend in proportion of tools added to the
#' scRNA-tools database over time against the overall proportion of tools in
#' the database.
#'
#' @param categories data.frame containing categories data
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_category_prop_trend <- function(categories, tools) {

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
        dplyr::filter(Present)

    tools_quarter <- tools %>%
        dplyr::mutate(Quarter = lubridate::quarter(Added, with_year = TRUE)) %>%
        dplyr::group_by(Quarter) %>%
        dplyr::count(name = "TotalCount")

    cats_quarter <- cats_dates %>%
        dplyr::mutate(Quarter = lubridate::quarter(Added, with_year = TRUE)) %>%
        dplyr::group_by(Quarter, Category) %>%
        dplyr::count(name = "Count") %>%
        dplyr::group_by(Quarter) %>%
        tidyr::complete(Quarter, Category, fill = list(Count = 0)) %>%
        dplyr::left_join(tools_quarter, by = "Quarter") %>%
        dplyr::mutate(Prop = Count / TotalCount) %>%
        dplyr::arrange(Quarter) %>%
        dplyr::mutate(
            QuarterYear = floor(Quarter),
            NumQuarter = dplyr::case_when(
                round(Quarter %% 1 * 10) == 1 ~ QuarterYear + 0.25,
                round(Quarter %% 1 * 10) == 2 ~ QuarterYear + 0.50,
                round(Quarter %% 1 * 10) == 3 ~ QuarterYear + 0.75,
                round(Quarter %% 1 * 10) == 4 ~ QuarterYear + 1.00,
            )
        ) %>%
        dplyr::ungroup()

    cats_slopes <- cats_quarter %>%
        dplyr::filter(Quarter < max(Quarter)) %>%
        dplyr::group_by(Category) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            model = purrr::map(data, ~ lm(Prop ~ NumQuarter, data = .x))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Slope = purrr::map_dbl(model, ~ .x$coefficients[2])) %>%
        dplyr::arrange(Slope) %>%
        dplyr::left_join(cats_summ, by = "Category")

    ggplot2::ggplot(
        cats_slopes,
        ggplot2::aes(x = Prop, y = Slope, label = Category, colour = Category)
    ) +
        ggplot2::geom_hline(yintercept = 0, colour = "red") +
        ggplot2::geom_point() +
        ggrepel::geom_text_repel() +
        ggplot2::labs(
            x = "Proportion of tools in database",
            y = "Trend in proportion over time"
        ) +
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
        ggplot2::aes(x = long, y = lat, group = group, fill = Prop)
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
