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
        py_plot,
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
            Category = glue::glue("{Category} ({Total})"),
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

#' Plot categories per tool
#'
#' Plot categories per tool divided by R/Python and year added to database
#'
#' @param categories_idx data.frame containing categories index
#' @param tools data.frame containing tools data
#'
#' @details
#' A violin plot is plotted for each year with points showing individual tools.
#' Large points indicate yearly means, connected by lines to show trends.
#'
#' @return ggplot object
plot_categories_per_tool <- function(categories_idx, tools) {

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
        dplyr::mutate(Year = lubridate::year(Added)) %>%
        dplyr::select(Tool, RPython, Category, Year) %>%
        dplyr::group_by(Tool, RPython, Year) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            RPython = factor(
                RPython,
                levels = c("R", "Both", "Python", "Other")
            )
        )

    means <- plot_data %>%
        dplyr::group_by(RPython, Year) %>%
        dplyr::summarise(Mean = mean(Count), .groups = "drop")

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
            x      = factor(Year),
            y      = Count,
            colour = RPython
        )
    ) +
        ggplot2::geom_violin(
            ggplot2::aes(fill = RPython),
            draw_quantiles = c(0.25, 0.5, 0.75),
            adjust = 2,
            alpha  = 0.4,
            colour = "white"
        ) +
        ggforce::geom_sina(alpha = 0.5, size = 0.5) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(y = Mean, group = RPython),
            size   = 2,
            colour = "white"
        ) +
        ggplot2::geom_line(
            data = means,
            ggplot2::aes(y = Mean, group = RPython),
            size = 1
        ) +
        ggplot2::geom_point(
            data = means,
            ggplot2::aes(y = Mean),
            size   = 4,
            colour = "white"
        ) +
        ggplot2::geom_point(
            data = means,
            ggplot2::aes(y = Mean),
            size   = 2,
            stroke = 1,
            fill   = "white"
        ) +
        ggplot2::facet_grid(RPython ~ .) +
        ggplot2::scale_y_continuous(breaks = seq(0, 20, 2)) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::labs(
            x = "Year added to database",
            y = "Number of categories"
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(
            legend.position  = "none",
            panel.grid.minor = ggplot2::element_blank()
        )
}

#' Plot add delay
#'
#' Plot delay between the first reference for a tools and when it was added to
#' the database
#'
#' @param tools data.frame with tools data
#' @param references data.frame with references data
#' @param doi_idx data.frame with DOI index
#'
#' @return ggplot2 object
plot_add_delay <- function(tools, references, doi_idx) {

    extrafont::loadfonts(quiet = TRUE)

    ref_dates <- doi_idx %>%
        dplyr::left_join(references, by = "DOI") %>%
        dplyr::group_by(Tool) %>%
        dplyr::summarise(FirstRefDate = min(Date, na.rm = TRUE)) %>%
        dplyr::select(Tool, FirstRefDate) %>%
        dplyr::left_join(tools, by = "Tool") %>%
        dplyr::mutate(Delay = as.numeric(Added - FirstRefDate)) %>%
        dplyr::mutate(
            DelayGroup = dplyr::case_when(
                is.infinite(Delay) ~ NA_character_,
                Delay < -365       ~ "More than 365 days before",
                Delay < -180       ~ "180 to 365 days before",
                Delay <  -30       ~ "30 to 180 days before",
                Delay <    0       ~ "Less than 30 days before",
                Delay <   30       ~ "Less than 30 days after",
                Delay <  180       ~ "30 to 180 days after",
                Delay <  365       ~ "180 to 365 days after",
                TRUE               ~ "More than 365 days after"
            )
        )

    min_delay <- min(ref_dates$Delay[is.finite(ref_dates$Delay)])
    max_delay <- max(ref_dates$Delay[is.finite(ref_dates$Delay)])

    scatter <- ggplot2::ggplot(
        ref_dates,
        ggplot2::aes(
            x = FirstRefDate, y = Added, colour = Delay, fill = Delay)
    ) +
        ggplot2::geom_point(size = 2, alpha = 0.5, shape = 21) +
        ggplot2::geom_abline(slope = 1, colour = "red") +
        ggplot2::scale_colour_stepsn(
            breaks = c(-365, -180, -30, 0, 30, 180, 365),
            values = scales::rescale(
                c(min_delay, -365, -180, -30, 0, 30, 180, 365, max_delay)
            ),
            colours = c("#7a0177", "#43a2ca", "#7bccc4", "#bae4bc", "white",
                        "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177"),
            guide = ggplot2::guide_coloursteps(
                even.steps  = TRUE,
                show.limits = TRUE,
                barheight   = 20
            )
        ) +
        ggplot2::scale_fill_stepsn(
            breaks = c(-365, -180, -30, 0, 30, 180, 365),
            values = scales::rescale(
                c(min_delay, -365, -180, -30, 0, 30, 180, 365, max_delay)
            ),
            colours = c("#7a0177", "#43a2ca", "#7bccc4", "#bae4bc", "white",
                        "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177"),
            guide = "none"
        ) +
        ggplot2::labs(
            x = "Date of first reference",
            y = "Date added to database",
            colour = "Delay\n(days)"
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(legend.position = "none")

    plot_data <- ref_dates %>%
        dplyr::group_by(DelayGroup) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            DelayGroup = forcats::fct_explicit_na(DelayGroup, "No reference"),
            DelayGroup = factor(
                DelayGroup,
                levels = c(
                    "No reference",
                    "More than 365 days before",
                    "180 to 365 days before",
                    "30 to 180 days before",
                    "Less than 30 days before",
                    "Less than 30 days after",
                    "30 to 180 days after",
                    "180 to 365 days after",
                    "More than 365 days after"
                )
            )
        ) %>% dplyr::mutate(
            hjust = dplyr::if_else(
                DelayGroup == "Less than 30 days after",
                1,
                0
            )
        ) %>%
        dplyr::mutate(Prop = Count / sum(Count)) %>%
        dplyr::mutate(
            PctStr = format(Prop * 100, digits = 1, nsmall = 1),
            Label  = glue::glue("**{DelayGroup}**<br/>{Count}, {PctStr}%")
        )

    bar_plot <- ggplot2::ggplot(
        dplyr::filter(plot_data, !is.na(DelayGroup)),
        ggplot2::aes(y = DelayGroup, x = Count, fill = DelayGroup)
    ) +
        ggplot2::geom_col() +
        ggtext::geom_richtext(
            ggplot2::aes(label = Label, hjust = hjust),
            size         = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            family       = "Noto Sans"
        ) +
        ggplot2::scale_fill_manual(
            values = c("grey50", "#675796", "#58afc0", "#91d2b8", "#d8f0d8",
                       "#ffd5d6", "#f985a3", "#d93e8b", "#941d76")
        ) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.5)) +
        ggplot2::scale_x_continuous(
            expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::labs(x = "Delay in adding to database") +
        theme_1000_bar(direction = "h", base_size = 16) +
        ggplot2::theme(
            axis.title.x = ggplot2::element_text(
                hjust  = 0, vjust = -4, face   = "bold"
            )
        )

    cowplot::plot_grid(
        scatter,
        bar_plot,
        rel_widths = c(4, 2),
        nrow       = 1,
        align      = "h",
        axis       = c("tb")
    )
}

#' Plot metric correlations
#'
#' Plot the correlation between metrics for publications and tools
#'
#' @param references data.frame containing references data
#' @param references data.frame containing tools data
#'
#' @return assembled ggplot2 object
plot_metric_correlations <- function(references, tools) {

    extrafont::loadfonts(quiet = TRUE)
    cowplot::set_null_device("agg")

    publications <- plot_publications_correlation(references)
    tools <- plot_tools_correlation(tools)

    cowplot::plot_grid(
        publications,
        tools,
        nrow        = 2,
        rel_heights = c(0.7, 1)
    )
}

#' Plot publications correlation
#'
#' Plot the correlation between citations and Altmetric attention score for
#' publications
#'
#' @param references data.frame containing references data
#'
#' @return ggplot2 object
plot_publications_correlation <- function(references) {

    extrafont::loadfonts(quiet = TRUE)

    plot_data <- references %>%
        dplyr::filter(
            !Preprint,
            Years > 0
        ) %>%
        dplyr::mutate(
            LogCitations = log10(Citations + 1),
            LogAltmetric = log10(Altmetric + 1)
        ) %>%
        dplyr::mutate(
            LogAltmetric = dplyr::if_else(
                is.na(LogAltmetric),
                -Inf,
                LogAltmetric
            )
        ) %>%
        dplyr::select(Citations, Altmetric, LogCitations, LogAltmetric, Years)

    fit <- lm(
        LogAltmetric ~ LogCitations,
        data = dplyr::filter(plot_data, is.finite(LogAltmetric))
    )
    int_str <- format(
        fit$coefficients[1],
        digits     = 2,
        nsmall     = 2,
        scientific = FALSE,
        trim       = FALSE
    )
    slope_str <- format(
        fit$coefficients[2],
        digits     = 2,
        scientific = FALSE,
        trim       = TRUE
    )

    rho <- cor(
        plot_data$LogCitations, plot_data$LogAltmetric,
        method = "spearman",
        use    = "complete.obs"
    )
    rho_str <- format(
        rho,
        digits     = 2,
        scientific = FALSE,
        trim       = TRUE
    )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = LogCitations, y = LogAltmetric, colour = Years)
    ) +
        ggplot2::geom_abline(
            slope    = 1,
            linetype = "dashed",
            colour   = "#f781bf",
            size     = 1
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(
            method  = "lm",
            formula = "y ~ x",
            colour  = "#4daf4a",
            fill    = "#4daf4a",
            alpha   = 0.2
        ) +
        withr::with_package("ggtext", {
            ggplot2::annotate(
                "richtext",
                x = 3.0, y = 0.5,
                label = glue::glue(
                    "Spearman's &rho; = {rho_str}",
                    "<br/>",
                    "y = {int_str} + {slope_str}x"
                ),
                size         = 4,
                hjust        = 0,
                fill         = NA,
                label.colour = NA,
                lineheight   = 1.5,
                family       = "Noto Sans Math"
            )
        }) +
        ggplot2::scale_colour_viridis_c(
            option = "plasma",
            name   = "Age<br/>(years)"
        ) +
        ggplot2::labs(
            title = "Publications metrics correlations",
            x     = "log<sub>10</sub>(Citations + 1)",
            y     = "log<sub>10</sub>(Altmetric attention score + 1)"
        ) +
        ggplot2::guides(
            colour = ggplot2::guide_colourbar(
                barheight = 15
            )
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(
            axis.title.x        = ggtext::element_markdown(size = 10),
            axis.title.y        = ggtext::element_markdown(size = 10),
            legend.title        = ggtext::element_markdown(),
            plot.title.position = "plot"
        )
}

#' Plot tools correlation
#'
#' Plot the correlation between citations, Altmetric attention score and
#' GitHub stars for tools
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot2 object
plot_tools_correlation <- function(tools) {

    plot_data <- tools %>%
        dplyr::mutate(
            LogTotalCitations = log10(TotalCitations + 1),
            LogTotalAltmetric = log10(TotalAltmetric + 1),
            LogGHStars        = log10(GHStars + 1)
        ) %>%
        dplyr::mutate(
            LogTotalCitations = dplyr::if_else(
                is.na(LogTotalCitations),
                -Inf,
                LogTotalCitations
            ),
            LogTotalAltmetric = dplyr::if_else(
                is.na(LogTotalAltmetric),
                -Inf,
                LogTotalAltmetric
            ),
            LogGHStars = dplyr::if_else(
                is.na(LogGHStars),
                -Inf,
                LogGHStars
            )
        ) %>%
        dplyr::select(
            TotalCitations, TotalAltmetric, GHStars, LogTotalCitations,
            LogTotalAltmetric, LogGHStars, GHAgeYears
        )

    plot_data_long <- purrr::map2_dfr(
        .x = c("LogTotalCitations", "LogTotalCitations", "LogTotalAltmetric"),
        .y = c("LogTotalAltmetric", "LogGHStars", "LogGHStars"),
        function(.x, .y) {
            tibble::tibble(
                Comparison = glue::glue("{.x}VS{.y}"),
                XType      = .x,
                XValue     = plot_data[[.x]],
                YType      = .y,
                YValue     = plot_data[[.y]],
                GHAgeYears = plot_data$GHAgeYears
            )
        }
    ) %>%
        dplyr::mutate(
            XType = factor(
                XType,
                levels = c("LogTotalCitations", "LogTotalAltmetric"),
                labels = c(
                    "log<sub>10</sub>(Total citations + 1)",
                    "log<sub>10</sub>(Total Altmetric attention score + 1)"
                )
            ),
            YType = factor(
                YType,
                levels = c("LogTotalAltmetric", "LogGHStars"),
                labels = c(
                    "log<sub>10</sub>(Total Altmetric attention score + 1)",
                    "log<sub>10</sub>(GitHub stars + 1)"
                )
            )
        )

    fits <- plot_data_long %>%
        dplyr::group_by(XType, YType) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            model = purrr::map(
                data,
                ~ lm(
                    YValue ~ XValue,
                    data = dplyr::filter(
                        .x,
                        is.finite(XValue),
                        is.finite(YValue)
                    )
                )
            ),
            Rho = purrr::map_dbl(
                data,
                ~ cor(
                    .x$XValue, .x$YValue,
                    method = "spearman",
                    use    = "complete.obs"
                )
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            Intercept = purrr::map_dbl(model, ~ .x$coefficients[1]),
            Slope     = purrr::map_dbl(model, ~ .x$coefficients[2]),
            IntStr    = format(
                Intercept,
                digits     = 2,
                nsmall     = 2,
                scientific = FALSE,
                trim       = FALSE
            ),
            SlopeStr  = format(
                Slope,
                digits     = 2,
                scientific = FALSE,
                trim       = TRUE
            ),
        ) %>%
        dplyr::mutate(
            RhoStr = format(
                Rho,
                digits     = 2,
                scientific = FALSE,
                trim       = TRUE
            )
        ) %>%
        dplyr::mutate(
            Label = glue::glue(
                "Spearman's &rho; = {RhoStr}",
                "<br/>",
                "y = {IntStr} + {SlopeStr}x"
            )
        )

    plot <- ggplot2::ggplot(
        plot_data_long,
        ggplot2::aes(x = XValue, y = YValue)
    ) +
        ggplot2::geom_abline(
            slope    = 1,
            linetype = "dashed",
            colour   = "#f781bf",
            size     = 1
        ) +
        ggplot2::geom_point(ggplot2::aes(colour = GHAgeYears)) +
        ggplot2::geom_smooth(
            method  = "lm",
            formula = "y ~ x",
            colour  = "#4daf4a",
            fill    = "#4daf4a",
            alpha   = 0.2
        ) +
        ggtext::geom_richtext(
            data = fits,
            ggplot2::aes(label = Label),
            x = 2.8, y = 0.5,
            size         = 3,
            hjust        = 0,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.5,
            family       = "Noto Sans Math"
        ) +
        ggplot2::facet_grid(YType ~ XType, switch = "both") +
        ggplot2::scale_colour_viridis_c(
            option = "plasma",
            name   = "GitHub age<br/>(years)"
        ) +
        ggplot2::labs(
            title = "Tools metrics correlations",
        ) +
        ggplot2::guides(
            colour = ggplot2::guide_colourbar(
                barheight = 15
            )
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(
            axis.title.x        = ggplot2::element_blank(),
            axis.title.y        = ggplot2::element_blank(),
            strip.text.x        = ggtext::element_markdown(
                size   = 10,
                face   = "bold",
                hjust  = 0,
                margin = ggplot2::margin()
            ),
            strip.text.y.left   = ggtext::element_markdown(
                size   = 10,
                face   = "bold",
                hjust  = 0,
                margin = ggplot2::margin()
            ),
            strip.placement     = "outside",
            legend.title        = ggtext::element_markdown(),
            plot.title.position = "plot"
        )

    grob <- ggplot2::ggplotGrob(plot)
    grob$grobs[[4]] <- grid::nullGrob()

    return(grob)
}
