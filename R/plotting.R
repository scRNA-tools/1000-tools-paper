#' Plot tools over time
#'
#' Plot the number of tools in the database over time
#'
#' @param tools data.frame containing tools data
#'
#' @return ggplot object
plot_tools_over_time <- function(tools) {

    date_totals <- get_date_totals(tools) %>%
        dplyr::mutate(Day = as.numeric(Date - min(Date))) %>%
        dplyr::mutate(Day2 = Day * Day)

    fit <- lm(Total ~ Day + Day2, data = date_totals)

    x2_coef_str <- format(
        fit$coefficients[3],
        digits     = 1,
        nsmall     = 1,
        scientific = FALSE,
        trim       = FALSE
    )
    x_coef_str <- format(
        fit$coefficients[2],
        digits     = 1,
        nsmall     = 1,
        scientific = FALSE,
        trim       = FALSE
    )
    int_coef_str <- format(
        fit$coefficients[1],
        digits     = 1,
        nsmall     = 1,
        scientific = FALSE,
        trim       = FALSE
    )
    fit_label <- glue::glue(
        "y = ",
        "{x2_coef_str}x<sup>2</sup> + ",
        "{x_coef_str}x + ",
        "{int_coef_str}"
    )

    date_totals <- dplyr::mutate(date_totals, FittedTotal = fit$fitted.values)

    ggplot2::ggplot(
        date_totals,
        ggplot2::aes(x = .data$Date, y = .data$Total)
    ) +
        annotate_pub_date() +
        ggplot2::geom_line(
            ggplot2::aes(y = .data$Total),
            size   = 2,
            colour = "#984ea3",
            alpha  = 0.5
        ) +
        ggplot2::geom_function(
            fun = function(date) {
                day <- as.numeric(date - min(date_totals$Date))
                predict(fit, newdata = data.frame(Day = day, Day2 = day * day))
            },
            linetype = "longdash",
            size     = 1.5,
            colour   = "#984ea3"
        ) +
        ggtext::geom_richtext(
            data = tibble::tibble(
                Date  = as.Date("2020-01-01"),
                Total = 150
            ),
            label        = fit_label,
            family       = "Noto Sans",
            size         = 5,
            colour       = "#984ea3",
            fill         = NA,
            label.colour = NA,
        ) +
        ggplot2::labs(
            y = "Number of tools in database"
        ) +
        theme_1000(base_size = 16)
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
                levels = c("R", "Py",     "CPP", "MATLAB", "Other"),
                labels = c("R", "Python", "C++", "MATLAB", "Other")
            )
        )

    fits <- platform_dates %>%
        dplyr::mutate(
            Percent = Prop * 100,
            Day     = Date - min(Date)
        ) %>%
        dplyr::group_by(Platform) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            model = purrr::map(data, ~ lm(Percent ~ Day, data = .x))
        ) %>%
        dplyr::ungroup() %>%
        plyr::mutate(
            Intercept = purrr::map_dbl(model, ~ .x$coefficients[1]),
            Slope     = purrr::map_dbl(model, ~ .x$coefficients[2]),
            IntStr    = format(
                Intercept,
                digits = 1,
                nsmall = 1,
                scientific = FALSE,
                trim = FALSE
            ),
            SlopeStr  = format(
                abs(Slope),
                digits = 1,
                scientific = FALSE,
                trim = TRUE
            ),
        ) %>%
        dplyr::mutate(
            Label = glue::glue(
                "**{Platform}**",
                "<br/>",
                "(y = {IntStr} {ifelse(Slope > 0, '+', '-')} {SlopeStr}x)"
            )
        ) %>%
        dplyr::mutate(
            Date = max(platform_dates$Date),
            Prop = (Intercept + Slope * (Date - min(platform_dates$Date))) / 100
        )

    ggplot2::ggplot(
        platform_dates,
        ggplot2::aes(x = Date, y = Prop, colour = Platform)
    ) +
        add_date_gridlines(platform_dates) +
        add_y_gridlines(
            ymin = 0, ymax = 0.8, ystep = 0.2,
            xmin = min(platform_dates$Date), xmax = max(platform_dates$Date)
        ) +
        ggplot2::geom_vline(
            xintercept = max(platform_dates$Date),
            colour = "grey60"
        ) +
        annotate_pub_date() +
        ggplot2::geom_smooth(method = "lm", formula = "y ~ x", se = TRUE) +
        ggplot2::geom_line(size = 1, alpha = 0.5) +
        ggtext::geom_richtext(
            data = fits,
            ggplot2::aes(label = Label),
            hjust        = 0,
            size         = 3,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1,
            family       = "Noto Sans Math"
        ) +
        ggplot2::scale_x_date(
            breaks = seq.Date(
                lubridate::as_date("2017-01-01"),
                lubridate::as_date("2021-01-01"),
                by = "year"
            ),
            labels = scales::date_format("%Y"),
            expand = ggplot2::expansion(mult = c(0, 0.25))
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::labs(
            x = "Date added to database",
            y = "Percentage of tools in database"
        ) +
        theme_1000(base_size = 16, grid = "none") +
        ggplot2::theme(
            legend.position = "none"
        )
}

#' Plot publication delay
#'
#' Create a scatter plot of days until publication against preprint date for
#' papers with a linked preprint. Right side shows boxplot and density plot of
#' publication delay.
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
        ggplot2::geom_point(size = 2, alpha = 0.8) +
        ggplot2::scale_y_continuous(breaks = seq(0, 1500, 250)) +
        ggplot2::scale_colour_viridis_c(
            option = "plasma",
            name   = "log<sub>10</sub>(Citations)"
        ) +
        ggplot2::labs(
           x = "Preprint date",
           y = "Days until publication"
        ) +
        ggplot2::guides(
           colour = ggplot2::guide_colourbar(
               barwidth = 15
           )
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(
            legend.position  = "bottom",
            legend.title     = ggtext::element_markdown(
                size = ggplot2::rel(1.2)
            )
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

    cowplot::plot_grid(
        scatter,
        boxplot,
        density,
        rel_widths = c(4, 0.5, 1),
        nrow = 1,
        align = "h",
        axis = c("tb")
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
            PctStr = format(Prop * 100, digits = 1, nsmall = 1),
            Label  = glue::glue("**{PubStatus}**<br/>{Count}, {PctStr}%")
        )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = PubStatus, y = Count)) +
        ggplot2::geom_col(ggplot2::aes(fill = PubStatus)) +
        ggtext::geom_richtext(
            ggplot2::aes(label = Label),
            vjust        = -0.1,
            size         = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            family       = "Noto Sans"
        ) +
        bar_scales() +
        theme_1000_bar()
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
            size    = 10,
            colour  = "#984ea3",
            hjust   = 1,
            nudge_x = -0.01
        ) +
        ggplot2::geom_text(
            ggplot2::aes(label = Stat),
            size    = 10,
            colour  = "grey30",
            hjust   = 0,
            nudge_x = 0.01
        ) +
        ggplot2::scale_x_discrete(
            expand = ggplot2::expansion(add = c(0.16, 0.2))
        ) +
        ggplot2::scale_y_discrete(
            expand = ggplot2::expansion(add = c(3, 2))
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )
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
        ggrepel::geom_text_repel(
            size              = 3,
            family            = "Noto Sans",
            segment.size      = 0.7,
            segment.alpha     = 0.5,
            segment.linetype  = "dotted",
            box.padding       = 0.2,
            segment.curvature = -0.1,
            segment.ncp       = 3,
            segment.angle     = 20,
            seed              = 1
        ) +
        ggplot2::scale_color_hue(l = 50) +
        ggplot2::labs(
            x = "Proportion of tools in database",
            y = "Trend in proportion over time"
        ) +
        theme_1000(base_size = 16) +
        ggplot2::theme(legend.position = "none")
}

#' Plot publications models
#'
#' Plot coefficients for publications models
#'
#' @param publications_models list containing publications models
#'
#' @return ggplot2 object
plot_publications_models <- function(publication_models) {

    term_labels <- c(
        "splines::ns(Years, df = 3)3" = "Years (3rd degree)",
        "splines::ns(Years, df = 3)2" = "Years (2nd degree)",
        "splines::ns(Years, df = 3)1" = "Years (1st degree)",
        "HasPreprintTRUE"             = "Has preprint",
        "log2(NumAuthors)"            = "log2(Num authors)",
        "log2(NumReferences + 1)"     = "log2(Num references + 1)"
    )

    models_df <- tidy_models(
        publication_models,
        types = c(
            citations     = "Citations",
            altmetric     = "Altmetric"
        )
    )

    plot_models(models_df, term_labels)
}

#' Plot tools models
#'
#' Plot coefficients for tools models
#'
#' @param tools_models list containing tools models
#'
#' @return ggplot2 object
plot_tools_models <- function(tools_models) {

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

    models_df <- tidy_models(
        tools_models,
        types = c(
            citations     = "Total citations",
            altmetric     = "Total altmetric",
            gh_popularity = "GitHub popularity"
        )
    )

    plot_models(models_df, term_labels)
}

#' Plot models
#'
#' @param models_df data.frame containing tidy model coefficients from
#' `tidy_models()`
#' @param term_labels Named character vector with labels for coefficient terms
#'
#' @return ggplot2 object
plot_models <- function(models_df, term_labels) {
    # models_df <- models_df %>%
    #     dplyr::mutate(
    #         Label = glue::glue(
    #             "{format(estimate, digits = 1, nsmall = 1)}",
    #             "±",
    #             "{format(std.error, digits = 1, nsmall = 1)}"
    #         )
    #     )

    # ggrepel::geom_label_repel(
    #     ggplot2::aes(
    #         label = Label,
    #         group = Type
    #     ),
    #     position           = ggplot2::position_dodge(0.4),
    #     family             = "Noto Sans",
    #     size               = 3,
    #     min.segment.length = 0,
    #     segment.size       = 0.7,
    #     segment.alpha      = 0.5,
    #     segment.linetype   = "dotted",
    #     box.padding        = 0.6,
    #     label.padding      = 0.15,
    #     point.padding      = 0.2,
    #     segment.curvature  = -0.1,
    #     segment.ncp        = 3,
    #     segment.angle      = 20,
    #     seed               = 1,
    #     show.legend        = FALSE
    # ) +

    ggplot2::ggplot(
        models_df,
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
            colour     = "#f781bf",
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
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::scale_shape_manual(values = c(21, 16)) +
        ggplot2::scale_size_manual(values = c(2.2, 3)) +
        ggplot2::labs(x = "Coefficient") +
        theme_1000(base_size = 14) +
        ggplot2::theme(
            axis.title.y    = ggplot2::element_blank(),
            legend.position = "bottom",
            legend.box      = "vertical",
            legend.margin   = ggplot2::margin(t = -8)
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
            Percent = Total / nrow(tools) * 100,
            PctStr  = format(Percent, digits = 1, nsmall = 1),
            Label   = glue::glue(
                "**{Platform}**<br/>{Total}<br/>{PctStr}%"
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
            hjust        = 0,
            nudge_x      = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            size         = 5.5,
            family       = "Noto Sans"
        ) +
        bar_scales(direction = "h", expansion_mult = 0.25) +
        ggplot2::labs(title = "Platforms", fill = "R/Python") +
        theme_1000_bar(direction = "h", base_size = 16)
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
            Percent = Total / nrow(tools) * 100,
            PctStr  = format(Percent, digits = 1, nsmall = 1),
            Label   = glue::glue(
                "**{License}**<br/>{Total}<br/>{PctStr}%"
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
            hjust        = 0,
            nudge_x      = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            size         = 4,
            family       = "Noto Sans"
        ) +
        bar_scales(direction = "h", expansion_mult = 0.22) +
        ggplot2::labs(title = "Licenses", fill = "R/Python") +
        theme_1000_bar(direction = "h", base_size = 16)
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
                levels = c("Bioconductor", "CRAN", "Multiple", "PyPI", "None")
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
            Percent = Total / nrow(tools) * 100,
            PctStr  = format(Percent, digits = 1, nsmall = 1),
            Label   = glue::glue(
                "**{Repo}**<br/>{Total}<br/>{PctStr}%"
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
            hjust        = 0,
            nudge_x      = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            size         = 5.5,
            family       = "Noto Sans"
        ) +
        bar_scales(direction = "h", expansion_mult = 0.25) +
        ggplot2::labs(title = "Repositories", fill = "R/Python") +
        theme_1000_bar(direction = "h", base_size = 16)
}

#' Plot categories bar
#'
#' Plot a bar chart showing how many tools are in different categories
#'
#' @param categories_idx data.frame containing categories index
#' @param category_descs data.frame containing category descriptions
#'
#' @return ggplot object
plot_categories_bar <- function(categories_idx, category_descs) {

    plot_data <- categories_idx %>%
        dplyr::group_by(Category) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Count) %>%
        dplyr::left_join(category_descs, by = "Category") %>%
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
            Category = factor(Category, levels = Category),
            Percent  = Count / length(unique(categories_idx$Tool)) * 100,
            Label = glue::glue("**{Category}** {Count}, {round(Percent, 1)}%")
        )

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Count, y = Category)
    ) +
        ggplot2::geom_col(fill = "#984ea3") +
        ggtext::geom_richtext(
            ggplot2::aes(
                label   = Label,
                hjust   = dplyr::if_else(Count == max(Count), 1, 0),
                colour  = Count == max(Count)
            ),
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.2,
            size         = 4.5,
            family       = "Noto Sans"
        ) +

        ggplot2::scale_colour_manual(
            values = c("black", "white"),
            guide = "none"
        ) +
        bar_scales(direction = "h", expansion_mult = 0.3) +
        ggplot2::facet_grid(
            Phase ~ .,
            scales = "free_y",
            space  = "free_y",
            switch = "y"
        ) +
        theme_1000_bar(direction = "h", base_size = 16) +
        ggplot2::theme(
            strip.placement = "outside",
            strip.text      = ggplot2::element_text(
                face   = "bold",
                margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1)
            )
        )
}

#' Plot words trend
#'
#' Plot the trend in word usage in abstracts over time
#'
#' @param references data.frame containing references data
#' @param sc_stopwords data.frame with column containing single-cell stopwords
#' @param top_words vector of top words to highlight
#'
#' @return ggplot object
plot_words_trend <- function(references, sc_stopwords, top_words) {

    ref_dates <- references %>%
        dplyr::filter(!is.na(Abstract)) %>%
        dplyr::arrange(Date) %>%
        dplyr::group_by(Date) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        tidyr::complete(
            Date = tidyr::full_seq(Date, 1),
            fill = list(Count = 0)
        ) %>%
        dplyr::mutate(RefTotal = cumsum(Count)) %>%
        dplyr::select(Date, RefTotal)

    word_dates <- references %>%
        dplyr::select(DOI, Date, Abstract) %>%
        dplyr::filter(!is.na(Abstract)) %>%
        dplyr::mutate(
            Abstract = stringr::str_remove_all(Abstract, "\\S*https?:\\S*"),
            Abstract = stringr::str_remove_all(
                Abstract,
                "\\b-?[0-9]\\d*(\\.\\d+)?\\b"
            )
        ) %>%
        tidytext::unnest_tokens(Word, Abstract) %>%
        dplyr::anti_join(tidytext::stop_words, by = c("Word" = "word")) %>%
        dplyr::anti_join(sc_stopwords, by = c("Word" = "word")) %>%
        dplyr::distinct() %>%
        dplyr::group_by(Date, Word) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(MaxDate = max(Date)) %>%
        dplyr::group_by(Word) %>%
        dplyr::filter(sum(Count) > 10) %>%
        tidyr::complete(
            Date = seq.Date(min(Date), unique(MaxDate), by = "day"),
            fill = list(Count = 0)
        ) %>%
        dplyr::select(-MaxDate) %>%
        dplyr::mutate(Total = cumsum(Count)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(ref_dates, by = "Date") %>%
        dplyr::mutate(Prop = Total / RefTotal) %>%
        dplyr::filter(Date >= "2017-01-01") %>%
        dplyr::group_by(Word) %>%
        dplyr::mutate(PropChange = Prop - Prop[1]) %>%
        dplyr::ungroup()

    top_data <- dplyr::filter(word_dates, Word %in% top_words)

    ggplot2::ggplot(
        word_dates,
        ggplot2::aes(x = Date, y = PropChange, group = Word)
    ) +
        add_date_gridlines(word_dates, minor = TRUE) +
        add_y_gridlines(
            ymin = -0.2, ymax = 0.2, ystep = 0.1,
            xmin = as.Date(-Inf), xmax = max(word_dates$Date)
        ) +
        ggplot2::geom_line(colour = "grey60", alpha = 0.5, size = 0.5) +
        ggplot2::annotate(
            "segment",
            x = as.Date(-Inf), xend = max(word_dates$Date),
            y = 0, yend = 0,
            colour = "red"
        ) +
        ggplot2::geom_vline(
            xintercept = max(word_dates$Date),
            colour     = "grey60"
        ) +
        ggplot2::geom_line(
            data = top_data,
            ggplot2::aes(colour = Word),
            size = 1
        ) +
        ggrepel::geom_text_repel(
            data = dplyr::filter(top_data, Date == max(Date)),
            ggplot2::aes(label = Word, colour = Word),
            direction          = "y",
            hjust              = 0,
            xlim = c(max(word_dates$Date) + 30, NA),
            size               = 3.2,
            family             = "Noto Sans",
            min.segment.length = 0,
            segment.size       = 0.7,
            segment.alpha      = 0.5,
            segment.linetype   = "dotted",
            box.padding        = 0.2,
            segment.curvature  = -0.1,
            segment.ncp        = 3,
            segment.angle      = 20,
            seed               = 1
        ) +
        ggplot2::scale_x_date(
            expand = ggplot2::expansion(mult = c(0.01, 0.1)),
            breaks = seq.Date(
                lubridate::as_date("2017-01-01"),
                lubridate::as_date("2021-01-01"),
                by = "year",
            ),
            labels = scales::date_format("%Y")
        ) +
        ggplot2::scale_colour_hue(l = 50) +
        ggplot2::labs(
            x = "Publication date",
            y = "Change in proportion of abstracts"
        ) +
        theme_1000(base_size = 16, grid = "none") +
        ggplot2::theme(
            legend.position = "none"
        )
}

#' Plot linked reference proportion
#'
#' Plot a bar chart showing the proportion of publications with linked preprints
#'
#' @param references data.frame containing references data
#' @param ref_links data.frame containing reference links data
#'
#' @return ggplot2 object
plot_linked_prop <- function(references, ref_links) {

    plot_data <- references %>%
        dplyr::filter(!Preprint) %>%
        dplyr::mutate(IsLinked = DOI %in% ref_links$Publication) %>%
        dplyr::group_by(IsLinked) %>%
        dplyr::count(name = "Count") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            Prop   = Count / sum(Count),
            PctStr = format(Prop * 100, digits = 1, nsmall = 1),
            Label  = glue::glue(
                "**{ifelse(IsLinked, 'Has preprint', 'No preprint')}**",
                "<br/>",
                "{Count}, {PctStr}%"
            )
        ) %>%
        dplyr::arrange(desc(IsLinked)) %>%
        dplyr::mutate(LabelPos = cumsum(Prop) - Prop * 0.5)

    ggplot2::ggplot(plot_data, ggplot2::aes(x = 1, y = Prop, fill = IsLinked)) +
        ggplot2::geom_col(position = "stack") +
        ggtext::geom_richtext(
            ggplot2::aes(y = LabelPos, label = Label),
            size         = 5,
            colour       = "white",
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.5,
            family       = "Noto Sans"
        ) +
        ggtext::geom_richtext(
            x = 1, y = 1,
            label = glue::glue("**Publications**<br/>{sum(plot_data$Count)}"),
            vjust        = 0,
            size         = 5,
            fill         = NA,
            label.colour = NA,
            lineheight   = 1.5,
            family       = "Noto Sans"
        ) +
        ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.1)) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.1, 0.2))
        ) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        theme_1000_bar() +
        ggplot2::theme(
            legend.position = "none",
            axis.line.x     = ggplot2::element_blank()
        )
}
