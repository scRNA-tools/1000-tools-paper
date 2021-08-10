#' Load tools
#'
#' @param file Path to tools TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing tools
load_tools <- function(file) {
    readr::read_tsv(
        file,
        col_types       = readr::cols(
            Tool        = readr::col_character(),
            Platform    = readr::col_character(),
            Code        = readr::col_character(),
            Description = readr::col_character(),
            License     = readr::col_character(),
            Added       = readr::col_date(format = ""),
            Updated     = readr::col_date(format = "")
        )
    )
}

#' Load category descriptions
#'
#' @param file Path to category descriptions TSV file, passed to
#' `readr::read_tsv()`
#'
#' @return tibble containing categories descriptions
load_category_descs <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            Category    = readr::col_character(),
            Phase       = readr::col_character(),
            Description = readr::col_character()
        )
    ) %>%
        dplyr::mutate(
            Phase = factor(
                Phase,
                levels = c(
                    "Phase 1",
                    "Phase 2",
                    "Phase 3",
                    "Phase 4",
                    "Multiple",
                    "Other"
                ),
                labels = c(
                    "Data\nacquisition",
                    "Data cleaning",
                    "Cell assignment",
                    "Gene\nidentification",
                    "Multiple",
                    "Other"
                )
            ),
        )
}

#' Load categories index
#'
#' @param file Path to categories index TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing categories index
load_categories_idx <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            Tool     = readr::col_character(),
            Category = readr::col_character()
        )
    )
}

#' Load references
#'
#' @param file Path to references TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing references
load_references <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            DOI      = readr::col_character(),
            arXiv    = readr::col_logical(),
            Preprint = readr::col_logical(),
            Date     = readr::col_character(),
            Title    = readr::col_character()
        )
    ) %>%
        update_references()
}

#' Load DOI index
#'
#' @param file Path to DOI index TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing DOI index
load_doi_idx <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            Tool = readr::col_character(),
            DOI  = readr::col_character()
        )
    )
}

#' Load reference links
#'
#' @param file Path to reference TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing DOI index
load_ref_links <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            Preprint    = readr::col_character(),
            Publication = readr::col_character(),
            Correct     = readr::col_logical()
        )
    )
}

#' Load repositories
#'
#' @param file Path to repositories TSV file, passed to `readr::read_tsv()`
#'
#' @return tibble containing repositories
load_repositories <- function(file) {
    readr::read_tsv(
        file,
        col_types = readr::cols(
            Tool   = readr::col_character(),
            Bioc   = readr::col_character(),
            CRAN   = readr::col_character(),
            PyPI   = readr::col_character(),
            Conda  = readr::col_logical(),
            GitHub = readr::col_character()
        )
    ) %>%
        dplyr::select(-Conda)
}

#' Load GitHub repositories
#'
#' @param repositories data.frame of tool repositories
#'
#' @return tibble containing GitHub repositories
load_github_repositories <- function(repositories) {

    # Clear missing repos log file
    missing_tsv <- fs::path(here::here("_cache"), "missing_repos.tsv")
    if (fs::file_exists(missing_tsv)) {
        fs::file_delete(missing_tsv)
    }

    repos <- repositories %>%
        dplyr::filter(!is.na(GitHub)) %>%
        dplyr::pull(GitHub) %>%
        unique()

    get_repo_info_slowly <- purrr::slowly(
        get_repo_info,
        rate = purrr::rate_delay(0.5)
    )

    purrr::map_dfr(repos, get_repo_info_slowly) %>%
        dplyr::mutate(
            # Add 1 to avoid negative scores
            IssueActivity = log10((ClosedIssues / AgeYears) + 1),
            IssueResponse = max(log10(MedianResponseDays), na.rm = TRUE) -
                log10(MedianResponseDays),
            Popularity = log10(
                4 * (Forks / AgeYears) +
                    (Stars / AgeYears) +
                    1
            )
        )
}

#' Load tools from SHA
#'
#' Load the tools table from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing tools
load_tools_sha <- function(sha) {
    load_tools(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/tools.tsv"
        )
    )
}

#' Load category descriptions from SHA
#'
#' Load the category descriptions from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing category descriptions
load_category_descs_sha <- function(sha) {
    load_category_descs(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/categories.tsv"
        )
    )
}

#' Load categories index from SHA
#'
#' Load the categories index from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing categories index
load_categories_idx_sha <- function(sha) {
    load_categories_idx(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/categories-idx.tsv"
        )
    )
}

#' Load references from SHA
#'
#' Load the references table from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing references
load_references_sha <- function(sha) {
    load_references(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/references.tsv"
        )
    )
}

#' Load DOI index from SHA
#'
#' Load the DOI index from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing DOI index
load_doi_idx_sha <- function(sha) {
    load_doi_idx(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/doi-idx.tsv"
        )
    )
}

#' Load reference links from SHA
#'
#' Load the reference links table from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing DOI index
load_ref_links_sha <- function(sha) {
    load_ref_links(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/reference-links.tsv"
        )
    )
}

#' Load repositories from SHA
#'
#' Load the repositories table from GitHub corresponding to a specific commit
#'
#' @param sha SHA hash corresponding to a git commit
#'
#' @return tibble containing repositories
load_repositories_sha <- function(sha) {
    load_repositories(
        glue::glue(
            "https://github.com/scRNA-tools/scRNA-tools/raw/",
            sha,
            "/database/repositories.tsv"
        )
    )
}

#' Load Google Analytics users
#'
#' Load user information for scRNA-tools.org
#'
#' @param date Final date to get results for
#'
#' @return tibble with user stats
load_ga_users <- function(date) {
    ua_users  <- get_ga_users("155271574", type = "UA",  to_date = "2020-11-16")
    ga4_users <- get_ga_users(252902717,   type = "GA4", to_date = date)

    dplyr::bind_rows(ua_users, ga4_users)
}

#' Load Google Analytics countries
#'
#' Load country information for scRNA-tools.org
#'
#' @param date Final date to get results for
#'
#' @return tibble with country stats
load_ga_countries <- function(date) {
    ua_countries  <- get_ga_countries("155271574", type = "UA",
                                      to_date = "2020-11-16")
    ga4_countries <- get_ga_countries(252902717,   type = "GA4", to_date = date)

    ua_countries %>%
        dplyr::full_join(
            ga4_countries,
            by     = "Country",
            suffix = c("UA", "GA4")
        ) %>%
        tidyr::replace_na(list(UsersUA = 0, UsersGA4 = 0)) %>%
        dplyr::mutate(Users = UsersUA + UsersGA4) %>%
        dplyr::mutate(Prop = Users / sum(Users)) %>%
        dplyr::mutate(Country = dplyr::case_when(
            Country == "Côte d’Ivoire"       ~ "Ivory Coast",
            Country == "Czechia"             ~ "Czech Republic",
            Country == "Macedonia (FYROM)"   ~ "Macedonia",
            Country == "Myanmar (Burma)"     ~ "Myanmar",
            Country == "Trinidad & Tobago"   ~ "Trinidad",
            Country == "United Kingdom"      ~ "UK",
            Country == "United States"       ~ "USA",
            Country == "U.S. Virgin Islands" ~ "Virgin Islands",
            TRUE                             ~ Country
        ))
}
