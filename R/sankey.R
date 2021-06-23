#' Get Sankey data
#'
#' Manipulate tools data to the form required for a Sankey plot
#'
#' @param tools data.frame containing tools data
#'
#' @return data.frame containing Sankey data
get_sankey_data <- function(tools) {
    tools %>%
        dplyr::mutate(
            License = dplyr::case_when(
                LicenseGPL      ~ "GPL",
                LicenseMIT      ~ "MIT",
                LicenseBSD      ~ "BSD",
                LicenseApache   ~ "Apache",
                LicenseArtistic ~ "Artistic",
                LicenseOther    ~ "Other",
                TRUE            ~ "None"
            ),
            Platform = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            Repo = dplyr::case_when(
                Bioc + CRAN + PyPI > 1 ~ "Multiple",
                Bioc                   ~ "Bioc",
                CRAN                   ~ "CRAN",
                PyPI                   ~ "PyPI",
                TRUE                   ~ "None"
            )
        ) %>%
        dplyr::select(Tool, License, Platform, Repo) %>%
        dplyr::group_by(License, Platform, Repo) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
            License = factor(
                License,
                levels = rev(c(
                    "GPL", "MIT", "BSD", "Apache", "Artistic", "Other", "None"
                ))
            ),
            Platform = factor(
                Platform,
                levels = rev(c("Python", "Both", "R", "Other"))
            ),
            Repo = factor(
                Repo,
                levels = rev(c("PyPI", "Multiple", "CRAN", "Bioc", "None"))
            )
        )
}

#' Plot Sankey
#'
#' Create a Sankey plot showing overlap between differents sets of categories
#'
#' @param data data.frame with data to plot. Each column should be a set of
#' categories, except for one column which contains a count. Each row gives the
#' number of items belonging to that combination of categories.
#' @param value Name of the column that contains counts
#' @param colour Name of the column to use for colouring diagonals
#' @param width Width of the category polygon blocks
#' @param space Vertical space between category polygon blocks as a proportion
#' of the total values
#'
#' @return ggplot object
plot_sankey <- function(data, value, colour, width = 0.1, space = 0.05) {

    sets <- colnames(data)[colnames(data) != value]

    spacer <- space * sum(data[[value]])

    axes <- data.frame(X = seq_along(sets), Y = sum(data[[value]]))

    set_polys <- purrr::map(seq_along(sets), function(.idx) {
        generate_set_polys(
            data,
            set    = sets[.idx],
            value  = value,
            x      = .idx,
            width  = width,
            spacer = spacer
        )
    }) %>%
        setNames(sets) %>%
        centre_polys()

    set_labels <- purrr::map(set_polys, function(.polys) {
        .polys %>%
            dplyr::group_by(Category) %>%
            dplyr::summarise(
                Total = unique(Total),
                X     = ceiling(min(X)),
                Y     = max(Y)
            ) %>%
            dplyr::mutate(
                PctStr = format(
                    Total / sum(Total) * 100,
                    digits = 1,
                    nsmall = 1
                ),
                Label = glue::glue("{Category} ({Total}, {PctStr}%)")
            )
    })

    diagonals <- purrr::map(seq_along(sets)[-1], function(.idx) {
        generate_diagonals(
            data,
            set_polys,
            from_set = sets[.idx - 1],
            to_set   = sets[.idx],
            value    = value,
            colour   = colour,
            x_to     = .idx,
            width    = width
        )
    })

    gg <- ggplot2::ggplot(axes, ggplot2::aes(x = X, y = Y))

    for (diags in diagonals) {
        gg <- gg +
            ggforce::geom_diagonal_wide(
                data = diags,
                ggplot2::aes(
                    group = factor(Group),
                    fill = forcats::fct_rev(Colour)
                ),
                alpha = 0.8
            )
    }

    for (set in sets) {
        gg <- gg +
            ggforce::geom_shape(data = set_polys[[set]]) +
            ggplot2::geom_text(
                data = set_labels[[set]],
                ggplot2::aes(label = Label),
                vjust = -0.5
            )
    }

    gg +
        ggplot2::scale_x_continuous(
            breaks   = seq_along(sets),
            labels   = sets,
            position = "top"
        ) +
        ggplot2::scale_fill_brewer(palette = "Dark2", name = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "bottom",
            panel.grid      = ggplot2::element_blank(),
            axis.title      = ggplot2::element_blank(),
            axis.text.x     = ggplot2::element_text(size = 20),
            axis.text.y     = ggplot2::element_blank()
        )
}

#' Generate set polygons
#'
#' Create a set of polygons representing the categories in a set
#'
#' @param data data.frame containing Sankey data
#' @param set Name of the column with the set to use
#' @param value Name of the column containing counts
#' @param x x-axis position of the set
#' @param width Width of the category polygon blocks
#' @param spacer Vertical space between category polygon blocks
#'
#' @return tibble of set polygons
generate_set_polys <- function(data, set, value, x = 1, width = 0.1,
                               spacer = 10) {

    spacers <- spacer * (seq_along(unique(data[[set]])) - 1)

    data %>%
        dplyr::group_by(.data[[set]]) %>%
        dplyr::summarise(Total = sum(.data[[value]])) %>%
        dplyr::arrange(.data[[set]]) %>%
        dplyr::mutate(Set = set) %>%
        dplyr::rename(Category = .data[[set]]) %>%
        dplyr::mutate(
            Top     = cumsum(Total) + spacers,
            Bottom  = Top - Total,
            Top2    = Top,
            Bottom2 = Bottom
        ) %>%
        tidyr::pivot_longer(
            cols = c(Top, Bottom, Top2, Bottom2),
            names_to = "YType",
            values_to = "Y"
        ) %>%
        dplyr::select(-YType) %>%
        dplyr::arrange(Y) %>%
        dplyr::mutate(
            X = rep(
                c(x - width, x + width, x + width, x - width),
                length(unique(Category))
            )
        )
}

#' Generate diagonals
#'
#' Create a set of coordinates describing the transitions between two sets
#'
#' @param data data.frame containing Sankey data
#' @param set_polys List of set polygon data.frames
#' @param from_set Name of the column with the starting set
#' @param to_set Name of the column with the ending set
#' @param value Name of the column containing counts
#' @param colour Name of the column to use for the colour of each diagonal
#' @param x_to x-axis position of the ending set
#' @param width Width of the category polygon blocks
#'
#' @return data.frame describing diagonals
generate_diagonals <- function(data, set_polys, from_set, to_set, value, colour,
                               x_to = 2, width = 0.1) {

    x_from <- x_to - 1 + width
    x_to   <- x_to - width

    y_froms <- set_polys[[from_set]] %>%
        dplyr::select(Category, YFrom = Y) %>%
        dplyr::group_by(Category) %>%
        dplyr::summarise(YFrom = min(YFrom), .groups = "drop")

    y_tos <- set_polys[[to_set]] %>%
        dplyr::select(Category, YTo = Y) %>%
        dplyr::group_by(Category) %>%
        dplyr::summarise(YTo = min(YTo), .groups = "drop")

    diags <- tibble::tibble(
        FromSet      = from_set,
        FromCategory = data[[from_set]],
        ToSet        = to_set,
        ToCategory   = data[[to_set]],
        Count        = data[[value]],
        Colour       = data[[colour]]
    ) %>%
        dplyr::group_by(FromSet, FromCategory, ToSet, ToCategory, Colour) %>%
        dplyr::summarise(Count = sum(Count), .groups = "drop") %>%
        dplyr::group_by(FromCategory) %>%
        dplyr::mutate(TotalFrom = cumsum(Count)) %>%
        dplyr::left_join(y_froms, by = c(FromCategory = "Category")) %>%
        dplyr::mutate(
            YFromBottom = YFrom + dplyr::lag(TotalFrom, default = 0),
            YFromTop    = YFromBottom + Count
        ) %>%
        dplyr::group_by(ToCategory) %>%
        dplyr::mutate(TotalTo = cumsum(Count)) %>%
        dplyr::left_join(y_tos, by = c(ToCategory = "Category")) %>%
        dplyr::mutate(
            YToBottom = YTo + dplyr::lag(TotalTo, default = 0),
            YToTop    = YToBottom + Count
        ) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(
            cols = c(YFromBottom, YFromTop, YToBottom, YToTop),
            names_to = "YType",
            values_to = "Y"
        ) %>%
        dplyr::group_by(FromSet, FromCategory, ToSet, ToCategory, Colour) %>%
        dplyr::mutate(
            X = c(x_from, x_from, x_to, x_to),
            Group = dplyr::cur_group_id()
        ) %>%
        dplyr::ungroup()
}

#' Centre polygons
#'
#' Vertically centre a collection of sets of polygons
#'
#' @param polys_list List of data.frames describing sets of polygons
#'
#' @return list of data.frames describing polygons with adjusted y-axis
#' positions
centre_polys <- function(polys_list) {

    max_y <- max(purrr::map_dbl(polys_list, ~ max(.x$Y)))

    purrr::map(polys_list, function(.polys) {
        diff <- max_y - max(.polys$Y)
        dplyr::mutate(.polys, Y = Y + 0.5 * diff)
    })
}
