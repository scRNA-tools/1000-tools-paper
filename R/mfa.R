get_mfa_variables <- function() {
    list(
        General = list(
            type          = "s",
            supplementary = TRUE,
            variables     = c(
                "AddedDays",
                "UpdatedDays"
            )
        ),
        Platforms = list(
            type          = "n",
            supplementary = FALSE,
            variables     = c(
                "PlatformR",
                "PlatformPy",
                "PlatformCPP",
                "PlatformMATLAB"
            )
        ),
        Licenses = list(
            type          = "n",
            supplementary = TRUE,
            variables     = c(
                "LicenseGPL",
                "LicenseMIT",
                "LicenseBSD",
                "LicenseApache",
                "LicenseArtistic",
                "LicenseOther"
            )
        ),
        Publications = list(
            type          = "s",
            supplementary = FALSE,
            variables     = c(
                "Publications",
                "Preprints",
                "TotalCitations",
                "MeanCitationsPerDay"
            )
        ),
        Repositories = list(
            type          = "n",
            supplementary = FALSE,
            variables     = c(
                "Bioc",
                "CRAN",
                "PyPI",
                "GitHub"
            )
        ),
        GitHub = list(
            type          = "s",
            supplementary = FALSE,
            variables     = c(
                "GHStars",
                "GHForks",
                # "GHAgeYears",
                "GHContributors",
                "GHCommits",
                # "GHIssues",
                # "GHClosedIssues",
                # "GHPctIssuesClosed",
                # "GHMedianResponseDays",
                # "GHMedianClosedDays",
                # "GHMedianResponseDays",
                "GHIssueActivity",
                "GHIssueResponse"
            )
        ),
        Categories = list(
            type          = "n",
            supplementary = FALSE,
            variables     = c(
                "CatAlignment",
                "CatAlleleSpecific",
                "CatAlternativeSplicing",
                "CatAssembly",
                "CatCellCycle",
                "CatClassification",
                "CatClustering",
                "CatDifferentialExpression",
                "CatDimensionalityReduction",
                "CatExpressionPatterns",
                "CatGeneFiltering",
                "CatGeneNetworks",
                "CatGeneSets",
                "CatHaplotypes",
                "CatImmune",
                "CatImputation",
                "CatIntegration",
                "CatInteractive",
                "CatMarkerGenes",
                "CatModality",
                "CatNormalisation",
                "CatOrdering",
                "CatPerturbations",
                "CatQualityControl",
                "CatQuantification",
                "CatRareCells",
                "CatSimulation",
                "CatStemCells",
                "CatTransformation",
                "CatUMIs",
                "CatVariableGenes",
                "CatVariants",
                "CatVisualisation"
            )
        )
    )
}

run_mfa <- function(data, variables, impute_comps = 5, comps = 30) {

    groups <- purrr::map_dbl(variables, ~ length(.x$variables))
    types  <- purrr::map_chr(variables, ~ .x$type)
    supps  <- which(purrr::map_lgl(variables, ~ .x$supplementary))
    names  <- names(variables)

    message("Imputing missing values...")
    completed <- missMDA::imputeMFA(
        data,
        ncp           = impute_comps,
        group         = groups,
        type          = types,
        num.group.sup = supps
    )

    message("Performing MFA...")
    FactoMineR::MFA(
        data,
        tab.comp      = completed,
        group         = groups,
        type          = types,
        name.group    = names,
        ncp           = comps,
        graph         = FALSE,
        num.group.sup = supps
    )
}
