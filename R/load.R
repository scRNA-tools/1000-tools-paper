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
