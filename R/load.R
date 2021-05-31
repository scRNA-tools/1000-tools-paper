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
