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
