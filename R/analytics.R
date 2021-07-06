#' Get Google Analytics users
#'
#' Get the number of users for scRNA-tools.org for various time periods from the
#' Google Analytics API
#'
#' @param property Google Analytics property ID
#' @param type Type of the property. Either "UA" for older Universal Analytics
#' or "GA4" for more recent Google Analytics 4.
#' @param to_date Final date to get results for
#'
#' @details
#' Results are slightly different depending on `type`, "UA" reports users while
#' "GA4" reports active users. See here for details https://support.google.com/analytics/answer/9408920?hl=en.
#' The monthly values for "UA" cover 30 days while "GA4" is only 28 days.
#'
#' @return tibble with user stats
get_ga_users <- function(property, type = c("UA", "GA4"),
                         to_date = lubridate::today()) {

    type <- match.arg(type)

    if (is(to_date, "Date")) {
        to_date <- as.character(to_date)
    }

    if (type == "UA") {
        from_date <- "2017-07-15"

        if (to_date > "2020-11-16") {
            message("Final date for UA is 2020-11-16, using this for to_date")
            to_date <- "2020-11-16"
        }

        metrics <- c("1dayUsers", "7dayUsers", "30dayUsers")
    } else if (type == "GA4") {
        from_date <- "2020-11-16"
        metrics <- c("active1dayUsers", "active7dayUsers", "active28dayUsers")
    }

    users <- purrr::map(
        metrics,
        ~ fetch_ga(
            property   = property,
            type       = type,
            metrics    = .x,
            dimensions = "date",
            date_range = c(from_date, to_date),
            limit      = -1
        )
    )

    users <- purrr::reduce(users, dplyr::left_join, by = "date")
    colnames(users) <- c("Date", "UsersDay", "UsersWeek", "UsersMonth")
    users <- dplyr::arrange(users, Date)
    users <- tibble::as_tibble(users)

    return(users)
}

#' Get Google Analytics countris
#'
#' Get the number of users per country for scRNA-tools.org from the Google
#' Analytics API
#'
#' @param property Google Analytics property ID
#' @param type Type of the property. Either "UA" for older Universal Analytics
#' or "GA4" for more recent Google Analytics 4.
#' @param to_date Final date to get results for
#'
#' @return tibble with country stats
get_ga_countries <- function(property, type = c("UA", "GA4"),
                             to_date = lubridate::today()) {

    type <- match.arg(type)

    if (is(to_date, "Date")) {
        to_date <- as.character(to_date)
    }

    if (type == "UA") {
        from_date <- "2017-07-15"

        if (to_date > "2020-11-16") {
            message("Final date for UA is 2020-11-16, using this for to_date")
            to_date <- "2020-11-16"
        }

        metrics <- "users"
    } else if (type == "GA4") {
        from_date <- "2020-11-16"
        metrics <- "totalUsers"
    }

    countries <- fetch_ga(
        property   = property,
        type       = type,
        metrics    = metrics,
        dimensions = "country",
        date_range = c(from_date, to_date),
        limit      = -1
    )

    colnames(countries) <- c("Country", "Users")
    countries <- dplyr::arrange(countries, Country)
    countries <- tibble::as_tibble(countries)

    return(countries)
}

#' Fetch Google Analytics
#'
#' Fetch results from the Google Analytics API. This is a wrapper around
#' `googleAnalyticsR::google_analytics()` and `googleAnalyticsR::ga_data()` that
#' handles switching between the two types.
#'
#' @param property Google Analytics property ID
#' @param type Type of the property. Either "UA" for older Universal Analytics
#' or "GA4" for more recent Google Analytics 4.
#' @param metrics Metrics to fetch
#' @param dimensions Dimensions to fetch
#' @param date_range Date range to fetch results for
#' @param limit Number of results to fetch. Set `-1` for all results.
#'
#' @return data.frame with API results
fetch_ga <- function(property, type, metrics, dimensions, date_range, limit) {
    switch (type,
        UA = googleAnalyticsR::google_analytics(
            viewId     = property,
            metrics    = metrics,
            dimensions = dimensions,
            date_range = date_range,
            max        = limit
        ),
        GA4 = googleAnalyticsR::ga_data(
            propertyId = property,
            metrics    = metrics,
            dimensions = dimensions,
            date_range = date_range,
            limit      = limit
        )
    )
}
