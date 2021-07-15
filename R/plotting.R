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
            by     = c(Publication = "DOI"),
            suffix = c("", "Is")
        ) %>%
        dplyr::select(
            Preprint, Publication, PreprintDate, PublicationDate = Date,
            Citations
        ) %>%
        dplyr::mutate(
            Delay = as.numeric(PublicationDate - PreprintDate, units = "days")
        ) %>%
        dplyr::filter(Delay > 0)

    scatter <- ggplot2::ggplot(
        delays,
        ggplot2::aes(x = PreprintDate, y = Delay, colour = log10(Citations))
    ) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_viridis_c(option = "plasma") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(
                colour = "grey30",
                fill   = "NA"
            ),
            legend.position = "bottom"
        )

    boxplot <- ggplot2::ggplot(delays, ggplot2::aes(y = Delay)) +
        ggplot2::geom_boxplot(colour = "grey30") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank()
        )

    density <- ggplot2::ggplot(delays, ggplot2::aes(y = Delay)) +
        ggplot2::geom_density(fill = "grey30", colour = NA) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank()
        )

    patchwork::wrap_plots(
        scatter,
        boxplot,
        density,
        widths = c(4, 0.5, 1)
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
#' @param gh_repos data.frame containing GitHub repositories data
#'
#' @return ggplot object
plot_gh_stats <- function(gh_repos) {

    n_commits <- sum(gh_repos$Commits, na.rm = TRUE)
    n_issues  <- sum(gh_repos$Issues, na.rm = TRUE)

    stats <- tibble::tribble(
                ~ Stat,                                 ~ Value,
               "Repos",                          nrow(gh_repos),
              "Owners",          length(unique(gh_repos$Owner)),
        "Contributors", length(unique(unlist(gh_repos$Logins))),
             "Commits",                               n_commits,
              "Issues",                                n_issues,
               "Hours",        (n_commits + n_issues) * 10 / 60
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

#' Plot publications models
#'
#' Plot coefficients for linear models predicting citations and Altmetric score
#' for publications using [ggstatsplot::ggcoefstats()].
#'
#' @param references data.frame containing references data
#' @param ref_links data.frame containing publication-preprint links
#'
#' @details
#' Uses a simplified version of the model from Fu and Hughey 10.7554/eLife.52646
#' which excludes terms about affiliation, last author and MeSH keywords. The
#' model for citations looks like:
#'
#' `log(Citations + 1) ~ log2(NumReferences + 1) + log2(NumAuthors) +
#' HasPreprint + splines::ns(Years, df = 3)`
#'
#' The Altmetric score model is the same with Altmetric score replacing
#' citations.
#'
#' @return ggplot2 object
plot_publications_models <- function(references, ref_links) {

    model_data <- references %>%
        dplyr::filter(
            !Preprint,
            Years > 0
        ) %>%
        dplyr::mutate(HasPreprint = DOI %in% ref_links$Publication) %>%
        dplyr::select(HasPreprint, Years, NumAuthors, NumReferences, Citations,
                      Altmetric)

    citations_model <- lm(
        log2(Citations + 1) ~
            log2(NumReferences + 1) +
            log2(NumAuthors) +
            HasPreprint +
            splines::ns(Years, df = 3),
        data = model_data
    ) %>%
        ggstatsplot::ggcoefstats(output = "tidy") %>%
        dplyr::mutate(Type = "Citations")

    altmetric_model <- lm(
        log2(Altmetric + 1) ~
            log2(NumReferences + 1) +
            log2(NumAuthors) +
            HasPreprint +
            splines::ns(Years, df = 3),
        data = dplyr::filter(model_data, !is.na(Altmetric))
    ) %>%
        ggstatsplot::ggcoefstats(output = "tidy") %>%
        dplyr::mutate(Type = "Altmetric")

    term_labels <- c(
        "splines::ns(Years, df = 3)3" = "Years (3rd degree)",
        "splines::ns(Years, df = 3)2" = "Years (2nd degree)",
        "splines::ns(Years, df = 3)1" = "Years (1st degree)",
        "HasPreprintTRUE"             = "Has preprint",
        "log2(NumAuthors)"            = "log2(Num authors)",
        "log2(NumReferences + 1)"     = "log2(Num references + 1)"
    )

    models <- dplyr::bind_rows(citations_model, altmetric_model)

    ggplot2::ggplot(
        models,
        ggplot2::aes(
            x      = estimate,
            y      = term,
            colour = Type,
            shape  = p.value < 0.05,
            size   = p.value < 0.05
        )
    ) +
        ggplot2::geom_vline(
            xintercept = 0,
            linetype   = "dashed",
            colour     = "red",
            size       = 1
        ) +
        ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = conf.low, xmax = conf.high),
            position = ggplot2::position_dodge(width = 0.5),
            size     = 0.5,
            height   = 0.2
        ) +
        ggplot2::geom_point(
            position = ggplot2::position_dodge2(width = 0.5),
            stroke   = 1,
            fill     = "white"
        ) +
        ggplot2::scale_y_discrete(labels = term_labels) +
        ggplot2::scale_shape_manual(values = c(21, 16)) +
        ggplot2::scale_size_manual(values = c(2.2, 3)) +
        ggplot2::labs(x = "Coefficient") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title.y    = ggplot2::element_blank(),
            legend.position = "bottom"
        )
}

#' Plot tools models
#'
#' Plot coefficients for linear models predicting citations, Altmetric score and
#' GitHub popularity for tools using [ggstatsplot::ggcoefstats()].
#'
#' @param tools data.frame containing tools data
#'
#' @details
#' Uses a model inspired by the model from Fu and Hughey 10.7554/eLife.52646
#' for publications. The model for citations looks like:
#'
#' `log(Citations + 1) ~ Platform + HasRepo + splines::ns(GHAgeYears, df = 3)`
#'
#' The Altmetric score and GitHub popularity models are the same with those
#' values replacing citations.
#'
#' @return ggplot2 object
plot_tools_models <- function(tools) {

    model_data <- tools %>%
        dplyr::mutate(
            Platform     = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            HasRepo       = Bioc | CRAN | PyPI,
            GHPopularity  = GHPopularity / log10(2) # Change to log base 2
        ) %>%
        dplyr::mutate(
            Platform = factor(
                Platform,
                levels = c("Other", "R", "Python", "Both")
            )
        ) %>%
        dplyr::select(
            Platform, HasRepo, GHAgeYears, TotalCitations, TotalAltmetric,
            GHPopularity
        )

    citations_model <- lm(
        log2(TotalCitations + 1) ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data = dplyr::filter(model_data, !is.na(TotalCitations))
    ) %>%
        ggstatsplot::ggcoefstats(output = "tidy") %>%
        dplyr::mutate(Type = "Citations")

    altmetric_model <- lm(
        log2(TotalAltmetric + 1) ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data = dplyr::filter(model_data, !is.na(TotalAltmetric))
    ) %>%
        ggstatsplot::ggcoefstats(output = "tidy") %>%
        dplyr::mutate(Type = "Altmetric")

    popularity_model <- lm(
        GHPopularity ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data =dplyr::filter(model_data, !is.na(GHPopularity))
    ) %>%
        ggstatsplot::ggcoefstats(output = "tidy") %>%
        dplyr::mutate(Type = "GHPopularity")

    term_labels <- c(
        "(Intercept)"                      = "(Intercept)",
        "PlatformR"                        = "Platform (R)",
        "PlatformPython"                   = "Platform (Python)",
        "PlatformBoth"                     = "Platform (Both)",
        "HasRepoTRUE"                      = "Has repository",
        "splines::ns(GHAgeYears, df = 3)3" = "Years (3rd degree)",
        "splines::ns(GHAgeYears, df = 3)2" = "Years (2nd degree)",
        "splines::ns(GHAgeYears, df = 3)1" = "Years (1st degree)"
    )

    models <- dplyr::bind_rows(
        citations_model, altmetric_model, popularity_model
    )

    ggplot2::ggplot(
        models,
        ggplot2::aes(
            x      = estimate,
            y      = term,
            colour = Type,
            shape  = p.value < 0.05,
            size   = p.value < 0.05
        )
    ) +
        ggplot2::geom_vline(
            xintercept = 0,
            linetype   = "dashed",
            colour     = "red",
            size       = 1
        ) +
        ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = conf.low, xmax = conf.high),
            position = ggplot2::position_dodge(width = 0.5),
            size     = 0.5,
            height   = 0.2
        ) +
        ggplot2::geom_point(
            position = ggplot2::position_dodge2(width = 0.5),
            stroke   = 1,
            fill     = "white"
        ) +
        ggplot2::scale_y_discrete(labels = term_labels) +
        ggplot2::scale_shape_manual(values = c(21, 16)) +
        ggplot2::scale_size_manual(values = c(2.2, 3)) +
        ggplot2::labs(x = "Coefficient") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title.y    = ggplot2::element_blank(),
            legend.position = "bottom"
        )
}

#' Plot platforms bar
#'
#' Plot a bar chart showing how many tools use different platforms
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_platforms_bar <- function(tools) {

    plot_data <- tools %>%
        dplyr::mutate(
            RPython = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            PlatformOther = !(PlatformR | PlatformPy | PlatformCPP |
                                  PlatformMATLAB)
        ) %>%
        dplyr::select(
            Tool,
            dplyr::starts_with("Platform"),
            -Platform,
            RPython
        ) %>%
        dplyr::group_by(RPython) %>%
        dplyr::summarise(dplyr::across(tidyselect:::where(is.logical), sum)) %>%
        tidyr::pivot_longer(
            -RPython,
            names_to  = "Platform",
            values_to = "Count"
        ) %>%
        dplyr::mutate(Platform = stringr::str_remove(Platform, "Platform")) %>%
        dplyr::group_by(Platform) %>%
        dplyr::mutate(Total = sum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            Platform = factor(
                Platform,
                levels = c("R", "Py", "Other", "MATLAB", "CPP"),
                labels = c("R", "Python", "Other", "MATLAB", "C++")
            ),
            Platform = forcats::fct_reorder(Platform, -Total),
            RPython = factor(
                RPython,
                levels = c("R", "Both", "Python", "Other")
            )
        )

    plot_labels <- plot_data %>%
        dplyr::select(-RPython, -Count) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
            Percent = Total / sum(Total) * 100,
            Label   = glue::glue(
                "**{Platform}**<br/>{Total}<br/>{round(Percent, 1)}%"
            )
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Count, y = forcats::fct_rev(Platform))
    ) +
        ggplot2::geom_col(ggplot2::aes(fill = RPython)) +
        ggtext::geom_richtext(
            data = plot_labels,
            ggplot2::aes(x = Total, label = Label),
            hjust = 0, nudge_x = 5,
            fill = NA, label.colour = NA,
            lineheight = 1.2
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_fill_discrete(name = "R/Python") +
        ggplot2::expand_limits(x = c(0, max(plot_data$Total) * 1.1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}

#' Plot licenses bar
#'
#' Plot a bar chart showing how many tools use different licenses
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_licenses_bar <- function(tools) {

    plot_data <- tools %>%
        dplyr::mutate(
            RPython = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            License = dplyr::case_when(
                LicenseGPL      ~ "GPL",
                LicenseMIT      ~ "MIT",
                LicenseBSD      ~ "BSD",
                LicenseApache   ~ "Apache",
                LicenseArtistic ~ "Artistic",
                LicenseOther    ~ "Other",
                TRUE            ~ "None"
            )
        ) %>%
        dplyr::select(Tool, License, RPython) %>%
        dplyr::group_by(RPython, License) %>%
        dplyr::count(name = "Count") %>%
        dplyr::group_by(License) %>%
        dplyr::mutate(Total = sum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            License = factor(
                License,
                levels = c(
                    "GPL", "MIT", "BSD", "Apache", "Artistic", "Other", "None"
                )
            ),
            RPython = factor(
                RPython,
                levels = c("R", "Both", "Python", "Other")
            )
        )

    plot_labels <- plot_data %>%
        dplyr::select(-RPython, -Count) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
            Percent = Total / sum(Total) * 100,
            Label   = glue::glue(
                "**{License}**<br/>{Total}<br/>{round(Percent, 1)}%"
            )
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Count, y = forcats::fct_rev(License))
    ) +
        ggplot2::geom_col(ggplot2::aes(fill = RPython)) +
        ggtext::geom_richtext(
            data = plot_labels,
            ggplot2::aes(x = Total, label = Label),
            hjust = 0, nudge_x = 5,
            fill = NA, label.colour = NA,
            lineheight = 1.2
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_fill_discrete(name = "R/Python") +
        ggplot2::expand_limits(x = c(0, max(plot_data$Total) * 1.1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}

#' Plot repositories bar
#'
#' Plot a bar chart showing how many tools are in different software
#' repositories
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_repositories_bar <- function(tools) {

    plot_data <- tools %>%
        dplyr::mutate(
            RPython = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            Repo = dplyr::case_when(
                Bioc + CRAN + PyPI > 1 ~ "Multiple",
                Bioc                   ~ "Bioconductor",
                CRAN                   ~ "CRAN",
                PyPI                   ~ "PyPI",
                TRUE                   ~ "None"
            )
        ) %>%
        dplyr::select(Tool, Repo, RPython) %>%
        dplyr::group_by(RPython, Repo) %>%
        dplyr::count(name = "Count") %>%
        dplyr::group_by(Repo) %>%
        dplyr::mutate(Total = sum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            Repo = factor(
                Repo,
                levels = c("PyPI", "Multiple", "Bioconductor", "CRAN", "None")
            ),
            RPython = factor(
                RPython,
                levels = c("R", "Both", "Python", "Other")
            )
        )

    plot_labels <- plot_data %>%
        dplyr::select(-RPython, -Count) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
            Percent = Total / sum(Total) * 100,
            Label   = glue::glue(
                "**{Repo}**<br/>{Total}<br/>{round(Percent, 1)}%"
            )
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Count, y = forcats::fct_rev(Repo))
    ) +
        ggplot2::geom_col(ggplot2::aes(fill = RPython)) +
        ggtext::geom_richtext(
            data = plot_labels,
            ggplot2::aes(x = Total, label = Label),
            hjust = 0, nudge_x = 5,
            fill = NA, label.colour = NA,
            lineheight = 1.2
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_fill_discrete(name = "R/Python") +
        ggplot2::expand_limits(x = c(0, max(plot_data$Total) * 1.1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}

#' Plot categories bar
#'
#' Plot a bar chart showing how many tools are in different categories
#'
#' @param categories_idx data.frame containing categories_idx
#'
#' @return ggplot object
plot_categories_bar <- function(categories_idx) {

    plot_data <- categories_idx %>%
        dplyr::group_by(Category) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Count) %>%
        dplyr::mutate(
            Category = stringr::str_replace_all(
                Category, "([[:upper:]])", " \\1"
            ),
            Category = stringr::str_trim(Category),
            Category = dplyr::if_else(
                Category == "U M Is", "UMIs", Category
            ),
            Category = factor(Category, levels = Category),
            Percent  = Count / length(unique(categories_idx$Tool)) * 100,
            Label = glue::glue("**{Category}** {Count}, {round(Percent, 1)}%")
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Count, y = Category)
    ) +
        ggplot2::geom_col() +
        ggtext::geom_richtext(
            ggplot2::aes(
                label   = Label,
                hjust   = dplyr::if_else(Count == max(Count), 1, 0),
                colour  = Count == max(Count)
            ),
            fill = NA, label.colour = NA,
            lineheight = 1.2
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_colour_manual(
            values = c("black", "white"),
            guide = "none"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
}
