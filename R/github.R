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

    message(glue::glue("Getting SHA for {repo}/{path} on {date}..."))
    gh::gh(query)[[1]]$sha
}

#' Get repository information
#'
#' Get information for a GitHub repository
#'
#' @param repo GitHub repository to query ("user/repo")
#'
#' @return tibble with repository information
get_repo_info <- function(repo) {

    message("Getting information for ", repo, "...")
    query <- glue::glue("GET /repos/{repo}")

    result <- try(gh::gh(query))

    if (is(result, "try-error")) {
        log_missing_repo(repo)

        return(
            tibble::tibble(
                Repo              = character(),
                Owner             = character(),
                Name              = character(),
                Created           = lubridate::ymd_hms(),
                Stars             = integer(),
                Forks             = integer(),
                AgeYears          = numeric(),
                Contributors      = integer(),
                Commits           = numeric(),
                Issues            = integer(),
                ClosedIssues      = integer(),
                PctIssuesClosed   = numeric(),
                MedianReponseDays = numeric(),
                MedianClosedDays  = numeric()
            )
        )
    }

    contributors <- get_repo_contributors(repo)
    issues <- get_repo_issues(repo)

    issues_summ <- issues %>%
        dplyr::mutate(
            TimeResponse = Response - Created,
            TimeClosed   = Closed - Created
        ) %>%
        dplyr::summarise(
            Issues             = dplyr::n(),
            ClosedIssues       = sum(State == "closed"),
            PctIssuesClosed    = ClosedIssues / Issues * 100,
            MedianResponseDays = as.numeric(
                median(TimeResponse, na.rm = TRUE),
                units = "days"
            ),
            MedianClosedDays   = as.numeric(
                median(TimeClosed, na.rm = TRUE),
                units = "days"
            )
        )

    tibble::tibble(
        Repo            = repo,
        Owner           = result$owner$login,
        Name            = result$name,
        Created         = result$created_at,
        Updated         = result$updated_at,
        Stars           = result$stargazers_count,
        Forks           = result$forks_count
    ) %>%
        dplyr::mutate(
            dplyr::across(c("Created", "Updated"), lubridate::as_datetime)
        ) %>%
        dplyr::mutate(
            AgeYears = as.numeric((lubridate::now() - Created) / 365)
        ) %>%
        dplyr::left_join(contributors, by = "Repo") %>%
        dplyr::bind_cols(issues_summ)
}

#' Log missing repository
#'
#' Log a GitHub repository as missing due to a failed API query, most likely
#' because the name has been changed.
#'
#' @param repo GitHub repository to log ("user/repo")
#'
#' @return invisibly the path to the log file
log_missing_repo <- function(repo) {
    log_file <- fs::path(here::here("_cache"), "missing_repos.tsv")

    missing <- tibble::tibble(Repo = repo)

    if (fs::file_exists(log_file)) {
        logged <- readr::read_tsv(
            log_file,
            col_types = readr::cols(
                Repo = readr::col_character()
            )
        )
        logged <- dplyr::bind_rows(logged, missing) %>%
            dplyr::distinct()
    } else {
        logged <- missing
    }

    warning(paste("GitHub repository", repo, "does not exist"))
    readr::write_tsv(logged, log_file)

    invisible(log_file)
}

#' Get repository contributors
#'
#' Get contributors for a GitHub repository
#'
#' @param repo GitHub repository to query ("user/repo")
#'
#' @return tibble with contributor information
get_repo_contributors <- function(repo) {
    message("Getting contributors for ", repo, "...")
    query <- glue::glue("GET /repos/{repo}/stats/contributors")
    result <- gh::gh(query)

    # If no results wait for GitHub to cache stats
    if (length(result) == 0) {
        message("Waiting for GitHub cache...")
        Sys.sleep(10)
        result <- gh::gh(query)
    }

    tibble::tibble(
        Repo         = repo,
        Contributors = length(result),
        Commits      = sum(purrr::map_dbl(result, ~ .x$total))
    )
}

#' Get repository issues
#'
#' Get issues for a GitHub repository
#'
#' @param repo GitHub repository to query ("user/repo")
#'
#' @return tibble with issue information
get_repo_issues <- function(repo) {

    message("Getting issues for ", repo, "...")
    cached_issues <- load_issues_cache(repo)
    message("Loaded ", nrow(cached_issues), " cached issues")

    query <- glue::glue("GET /repos/{repo}/issues")
    gh_issues <- gh::gh(query, state = "all", .limit = Inf)

    if (length(gh_issues) == 0) {
        return(
            tibble::tibble(
                ID       = numeric(),
                Number   = numeric(),
                State    = character(),
                Comments = numeric(),
                Created  = lubridate::ymd_hms(),
                Response = lubridate::ymd_hms(),
                Updated  = lubridate::ymd_hms(),
                Closed   = lubridate::ymd_hms()
            )
        )
    }

    gh_issues <- purrr::map_dfr(gh_issues, function(.issue) {
        closed <- .issue$closed_at
        if (is.null(closed)) {
            closed <- NA
        }

        tibble::tibble(
            ID       = .issue$id,
            Number   = .issue$number,
            User     = .issue$user$login,
            PR       = !is.null(.issue$pull_request),
            State    = .issue$state,
            Created  = .issue$created_at,
            Updated  = .issue$updated_at,
            Closed   = closed
        )
    }) %>%
        dplyr::filter(!PR) %>%
        dplyr::select(-PR) %>%
        dplyr::mutate(
            dplyr::across(
                c("Created", "Updated", "Closed"),
                lubridate::as_datetime
            )
        )
    message("Found ", nrow(gh_issues), " GitHub issues")

    new_issues <- gh_issues %>%
        dplyr::left_join(
            dplyr::select(cached_issues, Number, Cached = Updated),
            by = "Number"
        ) %>%
        dplyr::filter(is.na(Cached) | Updated != Cached) %>%
        dplyr::select(-Cached)

    cached_issues <- dplyr::filter(
        cached_issues,
        !(Number %in% new_issues$Number)
    )
    message("Using ", nrow(cached_issues), " cached issues")

    if (nrow(new_issues) == 0) {
        return(cached_issues)
    }

    add_comments <- function(...) {
        row <- tibble::tibble(...)

        comments <- get_issue_comments(repo, row$Number)

        row %>%
            dplyr::mutate(
                Comments = nrow(comments),
                Response = find_first_response(
                    comments,
                    row$User,
                    row$Closed
                )
            )
    }
    # Add delay to avoid GitHub rate limit
    add_comments_slowly <- purrr::slowly(
        add_comments,
        rate = purrr::rate_delay(1)
    )

    message("Checking ", nrow(new_issues), " new issues")
    new_issues <- new_issues %>%
        purrr::pmap_dfr(add_comments_slowly) %>%
        dplyr::mutate(Repo = repo) %>%
        dplyr::relocate(
            Repo, ID, Number, User, State, Comments, Created, Response, Updated,
            Closed
        )

    issues <- cached_issues %>%
        dplyr::bind_rows(new_issues) %>%
        dplyr::arrange(Number)

    save_issues_cache(repo, issues)

    return(issues)
}

#' Get issue comments
#'
#' Get comments for a GitHub issue
#'
#' @param repo GitHub repository to query ("user/repo")
#' @param issue Number for the issue
#'
#' @return tibble with comments information
get_issue_comments <- function(repo, issue) {
    message(glue::glue("Getting comments for {repo} issue {issue}..."))

    query <- glue::glue("GET /repos/{repo}/issues/{issue}/comments")
    comments <- gh::gh(query, state = "all", .limit = Inf)

    if (length(comments) == 0) {
        return(
            tibble::tibble(
                ID      = numeric(),
                User    = character(),
                Created = lubridate::ymd_hms(),
                Updated = lubridate::ymd_hms()
            )
        )
    }

    purrr::map_dfr(comments, function(.comment) {
        tibble::tibble(
            ID      = .comment$id,
            User    = .comment$user$login,
            Created = .comment$created_at,
            Updated = .comment$updated_at
        )
    }) %>%
        dplyr::arrange(Created)
}

#' Find first response
#'
#' Find the time of the first response to a GitHub issue
#'
#' @param comments data.frame of comments on an issue
#' @param user User name of the user who opened the issue
#' @param closed Time the issues was closed, `NA` if unknown
#'
#' @details
#' The first response time is the time of the first comment from someone other
#' than the issue author. If there are no replies then when the issue was
#' closed is used.
#'
#' @return time of first response
find_first_response <- function(comments, user, closed) {
    comments <- dplyr::filter(comments, User != user)

    if (nrow(comments) > 0) {
        response <- lubridate::as_datetime(comments$Created[1])
    } else {
        response <- closed
    }

    response
}

#' Load issues cache
#'
#' Load the cached issues for a GitHub repository
#'
#' @param repo GitHub repository to load ("user/repo")
#'
#' @return tibble with issues information
load_issues_cache <- function(repo) {

    path <- get_issues_cache_path(repo)

    if (!fs::file_exists(path)) {
        return(
            tibble::tibble(
                Repo     = character(),
                ID       = numeric(),
                Number   = numeric(),
                User     = character(),
                State    = character(),
                Comments = numeric(),
                Created  = lubridate::ymd_hms(),
                Response = lubridate::ymd_hms(),
                Updated  = lubridate::ymd_hms(),
                Closed   = lubridate::ymd_hms()
            )
        )
    }

    readr::read_tsv(
        path,
        col_types = readr::cols(
            Repo     = readr::col_character(),
            ID       = readr::col_double(),
            Number   = readr::col_double(),
            User     = readr::col_character(),
            State    = readr::col_character(),
            Comments = readr::col_double(),
            Created  = readr::col_datetime(format = ""),
            Response = readr::col_datetime(format = ""),
            Updated  = readr::col_datetime(format = ""),
            Closed   = readr::col_datetime(format = "")
        )
    )
}

#' Save issues cache
#'
#' Save the cached issues for a GitHub repository
#'
#' @param repo GitHub repository to save ("user/repo")
#' @param issues data.frame of issues
#'
#' @return invisibly the path to the cached file
save_issues_cache <- function(repo, issues) {

    path <- get_issues_cache_path(repo)

    if (!fs::dir_exists(fs::path_dir(path))) {
        fs::dir_create(fs::path_dir(path))
    }

    readr::write_tsv(issues, path)

    invisible(path)
}

#' Get issues cache path
#'
#' Get path to the issues caches for a GitHub repository
#'
#' @param repo GitHub repository to check ("user/repo")
#'
#' @return path to issues cache file
get_issues_cache_path <- function(repo) {

    repo_split <- stringr::str_split(repo, "/")[[1]]
    repo_std <- paste0(repo_split[1], "--", repo_split[[2]])

    fs::path(here::here("_cache", "issues"), paste0(repo_std, ".tsv"))
}
