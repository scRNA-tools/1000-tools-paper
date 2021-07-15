#' Get single-cell stop words
#'
#' Get a list of single-cell specific stop words
#'
#' @return tibble with column containing words
get_sc_stopwords <- function() {
    tibble::tibble(
        word = c(
            "cell",
            "data",
            "single",
            "seq",
            "cells",
            "scrna",
            "rna",
            "expression",
            "gene",
            "methods",
            "analysis",
            "datasets",
            "sequencing",
            "types",
            "based",
            "method",
            "genes",
            "type",
            "biological",
            "bioinformatics",
            "supplementary",
            "thousands"
        )
    )
}

#' Get top words
#'
#' Get a list of top words to highlight in text analysis plots
#'
#' @return vector of words
get_top_words <- function() {
    c(
        "clustering",
        "differentiation",
        "inference",
        "integration",
        "deep",
        "linear",
        "lineage",
        "scale",
        "time",
        "variability",
        "pseudotime",
        "specific",
        "learning",
        "individual",
        "batch",
        "multiple",
        "infer",
        "heterogeneity"
    )
}
