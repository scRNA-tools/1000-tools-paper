#' Get R dependencies
#'
#' Get dependencies for R packages. Only dependencies with other provided R
#' packages are returned.
#'
#' @param bioc_pkgs vector of Bioconductor packages
#' @param cran_pkgs vector of CRAN packages
#'
#' @return tibble with dependency edges
get_r_deps <- function(bioc_pkgs, cran_pkgs) {

    r_pkgs <- c(bioc_pkgs, cran_pkgs, "SingleCellExperiment", "SeuratObject")

    bioc_deps <- get_bioc_deps(bioc_pkgs)
    cran_deps <- get_cran_deps(cran_pkgs)

    bioc_deps %>%
        dplyr::bind_rows(cran_deps) %>%
        dplyr::filter(To %in% r_pkgs) %>%
        dplyr::arrange(From)
}

#' Get Bioconductor dependencies
#'
#' Get dependencies for Bioconductor packages. Only dependencies with other
#' provided R packages are returned.
#'
#' @param bioc_pkgs vector of Bioconductor packages
#' @param r_pkgs vector of R packages for filtering
#'
#' @return tibble with dependency edges
get_bioc_deps <- function(bioc_pkgs) {

    BiocPkgTools::buildPkgDependencyDataFrame() %>%
        dplyr::filter(Package %in% bioc_pkgs) %>%
        dplyr::rename(From = Package, To = dependency, Type = edgetype)

}

#' Get CRAN dependencies
#'
#' Get dependencies for CRAN packages. Only dependencies with other provided R
#'  packages are returned.
#'
#' @param cran_pkgs vector of CRAN packages
#' @param r_pkgs vector of R packages for filtering
#'
#' @return tibble with dependency edges
get_cran_deps <- function(cran_pkgs) {

    cran_db <- available.packages(repos = "http://cran.r-project.org")

    purrr::map_dfr(
        c("Depends", "Imports", "Suggests"),
        function(.type) {
            deps <- tools::package_dependencies(
                cran_pkgs,
                cran_db,
                which = .type
            )
            tibble::tibble(
                From = cran_pkgs,
                To   = deps,
                Type = .type
            )
    }) %>%
        tidyr::unnest(To)
}

#' Get PyPI dependencies
#'
#' Get dependencies for PyPI packages. Only dependencies with other provided
#' PyPI packages are returned.
#'
#' @param pypi_pkgs vector of PyPI packages
#' @param johnnydep_path Path to the **johnnydep** executable
#'
#' @details
#' Dependencies are retrieved from both the wheelodex API and the **johnnydep**
#' tool and combined. Wheelodex has results for fewer tools but is more complete
#' for extra dependencies.
#'
#' @return tibble with dependency edges
get_pypi_deps <- function(pypi_pkgs, johnnydep_path) {

    purrr::map_dfr(pypi_pkgs, function(.tool) {
        message("Getting wheelodex dependencies for ", .tool, "...")
        wheelodex_deps <- fetch_wheelodex_deps(.tool)
        message("Getting johnnydep dependencies for ", .tool, "...")
        johnnydep_deps <- fetch_johnnydep_deps(.tool, johnnydep_path)

        deps <- wheelodex_deps %>%
            dplyr::bind_rows(johnnydep_deps) %>%
            dplyr::distinct()

        message("Found ", nrow(deps), " dependencies")
        return(deps)
    }) %>%
        dplyr::filter(To %in% c(pypi_pkgs, "anndata"))
}

#' Fetch johnnydep packages
#'
#' Get dependencies for a PyPI package using the **johnnydep** Python tool
#' (https://pypi.org/project/johnnydep/)
#'
#' @param package Name of the package to get dependencies for
#' @param johnnydep_path Path to the **johnnydep** executable
#'
#' @return tibble with dependency edges
fetch_johnnydep_deps <- function(package, johnnydep_path) {

    dummy <- tibble::tibble(
        From = character(),
        To   = character(),
        Type = character()
    )

    result <- suppressWarnings(system2(
        johnnydep_path,
        glue::glue("--output-format json --fields=ALL --no-deps {package}"),
        stdout = TRUE,
        stderr = FALSE
    ))

    if ("status" %in% names(attributes(result)) &&
        attr(result, "status") == 1) {
        warning("Getting deps failed for ", package, call. = FALSE)
        return(dummy)
    }

    data <- jsonlite::fromJSON(result)

    requires <- data$requires[[1]] %>%
        stringr::str_remove(">=?.*")

    deps <- tibble::tibble(
        From = package,
        To   = requires,
        Type = "Requires"
    )

    if (length(data$extras_available[[1]]) > 0) {
        extras <- data$extras_available[[1]] %>%
            stringr::str_remove(">=?.*")

        deps <- dplyr::bind_rows(
            deps,
            tibble::tibble(
                From = package,
                To   = extras,
                Type = "Extra"
            )
        )
    }

    if (length(data$extras_requested[[1]]) > 0) {
        extras <- data$extras_requested[[1]] %>%
            stringr::str_remove(">=?.*")

        deps <- dplyr::bind_rows(
            deps,
            tibble::tibble(
                From = package,
                To   = extras,
                Type = "Extra"
            )
        )
    }

    return(deps)
}

#' Fetch wheelodex packages
#'
#' Get dependencies for a PyPI package using the wheelodex API
#' (https://www.wheelodex.org/json-api/)
#'
#' @param package Name of the package to get dependencies for
#'
#' @return tibble with dependency edges
fetch_wheelodex_deps <- function(package) {

    dummy <- tibble::tibble(
        From = character(),
        To   = character(),
        Type = character()
    )

    query <- glue::glue(
        "https://www.wheelodex.org/json/projects/{package}/data"
    )

    result <- httr::GET(query)

    data <- jsonlite::fromJSON(rawToChar(result$content))

    if ("message" %in% names(data) &&
        data$message == "No wheels found for project") {
        message("No wheels found")
        return(dummy)
    }

    if (is.null(data$data$dist_info$metadata$requires_dist)) {
        message("No dependencies found")
        return(dummy)
    }

    data$data$dist_info$metadata$requires_dist %>%
        dplyr::mutate(
            From = package,
            Type = dplyr::if_else(
                !is.na(marker) & stringr::str_detect(marker, "extra"),
                "Extra",
                "Requires"
            )
        ) %>%
        dplyr::select(From, To = name, Type)
}
