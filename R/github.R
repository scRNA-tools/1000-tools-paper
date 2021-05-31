#' Get path SHA
#'
#' Get the SHA hash corresponding to the most recent commit to a file. If
#' `date` is given only commits before that date will be considered.
#'
#' @param path Path to the file to get the hash for (without leading `/`)
#' @param repo GitHub repository to query ("user/repo")
#' @param date Date to consider commits up to (YYYY-MM-DD)
#
#' @return Character vector containing SHA hash
get_path_sha <- function(path, repo = "scRNA-tools/scRNA-tools",
                         date = lubridate::today()) {

    query <- glue::glue(
        "GET ",
        "/repos/{repo}/commits",
        "?until={date}",
        "&path=/{path}"
    )

    gh::gh(query)[[1]]$sha
}
