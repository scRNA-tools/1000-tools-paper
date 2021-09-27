#' Update references
#'
#' Add recent citation counts and better publication dates to a set of
#' references
#'
#' @param references data.frame containing references
#'
#' @return tibble with updated references
update_references <- function(references) {

    works <- purrr::map_dfr(references$DOI[!references$arXiv], function(.doi) {
        message("Getting DOI ", .doi, "...")

        cr_works <- rcrossref::cr_works(dois = .doi)$data %>%
            dplyr::mutate(DOI = .doi)

        altmetric <- try(
            rAltmetric::altmetrics(doi = .doi) %>% rAltmetric::altmetric_data()
        )

        if (is(altmetric, "try-error")) {
            cr_works$Altmetric <- NA
        } else {
            cr_works$Altmetric <- as.numeric(altmetric$score)
        }

        cr_works
    })

    works <- works %>%
        dplyr::mutate(
            DateOnline = approx_date(published.online),
            DatePrint  = approx_date(published.print),
            DateIssued = approx_date(issued),
            Date       = dplyr::case_when(
                !is.na(DateOnline) ~ DateOnline,
                !is.na(DatePrint)  ~ DatePrint,
                TRUE               ~ DateIssued
            )
        ) %>%
        dplyr::mutate(
            Abstract = abstract %>%
                stringr::str_remove_all("<jats:title>.*?</jats:title>") %>%
                stringr::str_remove_all("<[^>]*>") %>%
                stringr::str_squish()
        ) %>%
        dplyr::mutate(
            NumAuthors      = purrr::map_dbl(
                author,
                ~ dplyr::if_else(is.null(.x), NA_integer_, nrow(.x))
            ),
            NumReferences   = as.numeric(reference.count),
            Days            = as.numeric(lubridate::today() - Date),
            Years           = Days / 365.25,
            Citations       = as.numeric(is.referenced.by.count),
            CitationsPerDay = Citations / Days
        ) %>%
        dplyr::select(
            DOI, Date, Abstract, Days, Years, NumAuthors, NumReferences,
            Citations, CitationsPerDay, Altmetric
        )

    references %>%
        dplyr::select(-Date) %>%
        dplyr::left_join(works, by = "DOI")
}

#' Find approximate date
#'
#' Convert a partial date string to an approximate date
#'
#' @param date_str Partial date string vector
#'
#' @return date vector
approx_date <- function(date_str) {
    lubridate::as_date(
        lubridate::parse_date_time(date_str, c("y-m-d", "y-m", "y"))
    )
}
