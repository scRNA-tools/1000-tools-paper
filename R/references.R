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
        rcrossref::cr_works(dois = .doi)$data
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
            Citations       = as.numeric(is.referenced.by.count),
            CitationsPerDay = Citations / as.numeric(lubridate::today() - Date)
        ) %>%
        dplyr::select(DOI = doi, Date, Citations, CitationsPerDay)

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
