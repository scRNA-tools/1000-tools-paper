#' Augment tools
#'
#' Add additional information to a tools dataset
#'
#' @param tools data.frame containing tools data
#'
#' @return tools tibble with additional information
augment_tools <- function(tools) {
    tools %>%
        expand_platforms() %>%
        expand_licenses() %>%
        add_timestamp_days()
}

#' Expand platforms
#'
#' Expand the `Platforms` column into a series of logical platform indicators
#'
#' @param tools data.frame containing tools data
#'
#' @return tools tibble with new logical platform columns
expand_platforms <- function(tools) {
    tools %>%
        dplyr::mutate(
            PlatformR      = stringr::str_detect(.data$Platform, "R"),
            PlatformPy     = stringr::str_detect(.data$Platform, "Python"),
            PlatformCPP    = stringr::str_detect(.data$Platform, "C++"),
            PlatformMATLAB = stringr::str_detect(.data$Platform, "MATLAB")
        )
}

#' Expand licenses
#'
#' Expand the `License` column into a series of logical license indicators
#'
#' @param tools data.frame containing tools data
#'
#' @return tools tibble with new logical license columns
expand_licenses <- function(tools) {
    tools %>%
        dplyr::mutate(
            LicenseGPL      = stringr::str_detect(.data$License, "GPL"),
            LicenseMIT      = stringr::str_detect(.data$License, "MIT"),
            LicenseBSD      = stringr::str_detect(.data$License, "BSD"),
            LicenseApache   = stringr::str_detect(.data$License, "Apache"),
            LicenseArtistic = stringr::str_detect(.data$License, "Artistic")
        ) %>%
        dplyr::mutate(
            LicenseOther = !is.na(License) & !(
                LicenseGPL | LicenseMIT | LicenseBSD | LicenseApache |
                    LicenseArtistic
            )
        )
}

#' Add timestamp days
#'
#' Add the number of days since the start of the database when each tool was
#' added or last updated
#'
#' @param tools data.frame containing tools data
#'
#' @return tools tibble with new `AddedDays` and `UpdatedDays` columns
add_timestamp_days <- function(tools) {
    tools %>%
        dplyr::mutate(
            AddedDays   = as.numeric(Added - min(Added)),
            UpdatedDays = as.numeric(Updated - min(Added))
        )
}
