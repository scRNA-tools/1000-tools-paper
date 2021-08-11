#' Model publications
#'
#' Fit linear models predicting citations and Altmetric score for publications
#' using
#'
#' @param references data.frame containing references data
#' @param ref_links data.frame containing publication-preprint links
#'
#' @details
#' Uses a simplified version of the model from Fu and Hughey 10.7554/eLife.52646
#' which excludes terms about affiliation, last author and MeSH keywords. The
#' model for citations looks like:
#'
#' `log(Citations + 1) ~ log2(NumReferences + 1) + log2(NumAuthors) +
#' HasPreprint + splines::ns(Years, df = 3)`
#'
#' The Altmetric score model is the same with Altmetric score replacing
#' citations.
#'
#' @return list of lm models
model_publications <- function(references, ref_links) {

    model_data <- references %>%
        dplyr::filter(
            !Preprint,
            Years > 0
        ) %>%
        dplyr::mutate(HasPreprint = DOI %in% ref_links$Publication) %>%
        dplyr::select(
            HasPreprint, Years, NumAuthors, NumReferences, Citations, Altmetric
        )

    citations_model <- lm(
        log2(Citations + 1) ~
            log2(NumReferences + 1) +
            log2(NumAuthors) +
            HasPreprint +
            splines::ns(Years, df = 3),
        data = model_data
    )

    altmetric_model <- lm(
        log2(Altmetric + 1) ~
            log2(NumReferences + 1) +
            log2(NumAuthors) +
            HasPreprint +
            splines::ns(Years, df = 3),
        data = dplyr::filter(model_data, !is.na(Altmetric))
    )

    list(
        citations = citations_model,
        altmetric = altmetric_model
    )
}

#' Model tools
#'
#' Fit linear models predicting citations, Altmetric score and GitHub popularity
#'  for tools using.
#'
#' @param tools data.frame containing tools data
#'
#' @details
#' Uses a model inspired by the model from Fu and Hughey 10.7554/eLife.52646
#' for publications. The model for citations looks like:
#'
#' `log(Citations + 1) ~ Platform + HasRepo + splines::ns(GHAgeYears, df = 3)`
#'
#' The Altmetric score and GitHub popularity models are the same with those
#' values replacing citations.
#'
#' @return list of lm models
model_tools <- function(tools) {

    model_data <- tools %>%
        dplyr::mutate(
            Platform     = dplyr::case_when(
                PlatformR & PlatformPy ~ "Both",
                PlatformR              ~ "R",
                PlatformPy             ~ "Python",
                TRUE                   ~ "Other"
            ),
            HasRepo       = Bioc | CRAN | PyPI,
            GHPopularity  = GHPopularity / log10(2) # Change to log base 2
        ) %>%
        dplyr::mutate(
            Platform = factor(
                Platform,
                levels = c("Other", "R", "Python", "Both")
            )
        ) %>%
        dplyr::select(
            Platform, HasRepo, GHAgeYears, TotalCitations, TotalAltmetric,
            GHPopularity
        )

    citations_model <- lm(
        log2(TotalCitations + 1) ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data = dplyr::filter(model_data, !is.na(TotalCitations))
    )

    altmetric_model <- lm(
        log2(TotalAltmetric + 1) ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data = dplyr::filter(model_data, !is.na(TotalAltmetric))
    )

    popularity_model <- lm(
        GHPopularity ~
            Platform +
            HasRepo +
            splines::ns(GHAgeYears, df = 3),
        data = dplyr::filter(model_data, !is.na(GHPopularity))
    )

    list(
        citations     = citations_model,
        altmetric     = altmetric_model,
        gh_popularity = popularity_model
    )
}

#' Tidy models
#'
#' Tidy a set of linear models
#'
#' @param models Named list of lm model objects
#' @param types Named vector of types (i.e. the predicted variable) for each
#' model. Must include items for all the names in `models`.
#'
#' @details
#' Tidy output comes from `ggstatsplot::ggcoefstats(..., output = "tidy")`
#'
#' @return tibble containing tidy model coefficients
tidy_models <- function(models, types) {

    if (!all(names(models) %in% names(types))) {
        rlang::abort(paste0(
            "Some models do not have a matching type: ",
            paste(
                names(models)[!(names(models) %in% names(types))],
                collapse = ", "
            )
        ))
    }

    purrr::map_dfr(names(models), function(.model) {
        ggstatsplot::ggcoefstats(models[[.model]], output = "tidy") %>%
            dplyr::mutate(Type = types[.model])
    }) %>%
        dplyr::mutate(Type = factor(Type, levels = types))
}
