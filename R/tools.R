#' Augment tools
#'
#' Add additional information to a tools dataset
#'
#' @param tools data.frame containing tools data
#' @param references data.frame containing references data
#' @param doi_idx data.frame containing DOI index
#' @param repositories data.frame containing repositories data
#' @param gh_repos data.frame containing GitHub repositories data
#'
#' @return tools tibble with additional information
augment_tools <- function(tools, references, doi_idx, repositories, gh_repos) {
    tools %>%
        expand_platforms() %>%
        expand_licenses() %>%
        add_timestamp_days() %>%
        add_references(references, doi_idx) %>%
        add_repositories(repositories, gh_repos)
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
            LicenseGPL      = tidyr::replace_na(.data$LicenseGPL, FALSE),
            LicenseMIT      = tidyr::replace_na(.data$LicenseMIT, FALSE),
            LicenseBSD      = tidyr::replace_na(.data$LicenseBSD, FALSE),
            LicenseApache   = tidyr::replace_na(.data$LicenseApache, FALSE),
            LicenseArtistic = tidyr::replace_na(.data$LicenseArtistic, FALSE)
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

#' Add references
#'
#' Add a summary of references to the tools data
#'
#' @param tools data.frame containing tools data
#' @param references data.frame containing references data
#' @param doi_idx data.frame containing DOI index
#'
#' @return tools tibble with references summary
add_references <- function(tools, references, doi_idx) {

    references_summ <- doi_idx %>%
        dplyr::left_join(references, by = "DOI") %>%
        dplyr::group_by(Tool) %>%
        dplyr::summarise(
            Publications        = sum(!Preprint),
            Preprints           = sum(Preprint),
            TotalCitations      = sum(Citations, na.rm = TRUE),
            MeanCitationsPerDay = mean(CitationsPerDay, na.rm = TRUE)
        )

    tools %>%
        dplyr::left_join(references_summ, by = "Tool")
}

#' Add repositories
#'
#' Add repositories and GitHub stats to the tools data
#'
#' @param tools data.frame containing tools data
#' @param repositories data.frame containing repositories data
#' @param gh_repos data.frame containing GitHub repositories data
#'
#' @return tools tibble with repositories information
add_repositories <- function(tools, repositories, gh_repos) {

    colnames(gh_repos) <- paste0("GH", colnames(gh_repos))

    tools %>%
        dplyr::left_join(repositories, by = "Tool") %>%
        dplyr::left_join(gh_repos, by = c(GitHub = "GHRepo")) %>%
        dplyr::mutate(
            Bioc   = !is.na(Bioc),
            CRAN   = !is.na(CRAN),
            PyPI   = !is.na(PyPI),
            GitHub = !is.na(GitHub)
        )
}
