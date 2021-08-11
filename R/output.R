#' Save data tables
#'
#' Save a set of data.frames to a directory
#'
#' @param ... Named arguments with data.frames to save. Names will be used as
#' file names.
#' @param dir Output directory
#' @param strict If `TRUE` function will error if `is.data.frame()` is `FALSE`
#' for any items in `...`
#' @param clear Whether to delete files in `dir` before writing files. MUST BE
#' USED WITH CAUTION!
#'
#' @return `fs::dir_ls(dir)` invisibly
save_data_tables <- function(..., dir, strict = FALSE, clear = FALSE) {

    data_list <- rlang::list2(...)
    is_df <- purrr::map_lgl(data_list, is.data.frame)

    if (any(!is_df)) {
        if (strict) {
            rlang::abort(glue::glue(
                "Some items are not data.frames: {names(data_list)[!is_df]}"
            ))
        } else {
            rlang::inform(glue::glue(
                "Some items are not data.frames and will be ignored: ",
                "{names(data_list)[!is_df]}"
            ))
            data_list <- data_list[is_df]
        }
    }

    if (clear && fs::dir_exists(dir)) {
        rlang::inform(glue::glue("Clearing files in {dir}..."))
        fs::dir_walk(dir, function(.file) {
            rlang::inform(glue::glue("Deleting {.file}..."))
            fs::file_delete(.file)
        }, type = "file")
    }

    if (!fs::dir_exists(dir)) {
        message(dir, " does not exist, creating...")
        fs::dir_create(dir)
    }

    purrr::walk(names(data_list), function(.name) {
        df <- data_list[[.name]]
        has_list_cols <- any(sapply(df, is.list))

        if (has_list_cols) {
            path <- fs::path(dir, .name, ext = "Rds")
            rlang::inform(glue::glue("Writing {path}..."))
            readr::write_rds(data_list[[.name]], path, compress = "gz")
        } else {
            path <- fs::path(dir, .name, ext = "tsv")
            rlang::inform(glue::glue("Writing {path}..."))
            readr::write_tsv(data_list[[.name]], path)
        }
    })

    message("Done!")
    invisible(fs::dir_ls(dir))
}

#' Save model coefficients
#'
#' Write model coefficients to a TSV file
#'
#' @param models_df data.frame containing tidy model coefficients
#' @param path Path to TSV file
#'
#' @details
#' Output is a wide table where rows are terms, columns are models and values
#' are coefficients with confidence intervals
#'
#' @return `path` invisibly
save_model_coefficients <- function(models_df, path) {

    coefs_table <- models_df %>%
        dplyr::select(Type, Term = term, estimate, conf.low, conf.high) %>%
        tidyr::pivot_longer(
            c("estimate", "conf.low", "conf.high"),
            names_to  = "Estimate",
            values_to = "Value"
        ) %>%
        dplyr::mutate(ValueStr = format(Value, digits = 1, trim = TRUE)) %>%
        dplyr::select(-Value) %>%
        tidyr::pivot_wider(
            id_cols     = c("Type", "Term"),
            names_from  = "Estimate",
            values_from = "ValueStr"
        ) %>%
        dplyr::mutate(
            CoefStr = glue::glue("{estimate} ({conf.low}, {conf.high})")
        ) %>%
        dplyr::select(Type, Term, CoefStr) %>%
        tidyr::pivot_wider(names_from = "Type", values_from = "CoefStr")

    dir <- fs::path_dir(path)
    if (!fs::dir_exists(dir)) {
        rlang::inform(glue::glue("Creating {dir}..."))
        fs::dir_create(dir)
    }

    readr::write_tsv(coefs_table, path)

    invisible(path)
}
