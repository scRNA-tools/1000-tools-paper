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
        ggplot2::geom_vline(
            xintercept = lubridate::ymd("2018-06-25"),
            colour = "red"
        ) +
        ggplot2::annotate(
            "text",
            x = lubridate::ymd("2018-06-06"),
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
