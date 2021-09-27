#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

library(targets)
library(here)
library(magrittr)

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

source(here("R", "github.R"))
source(here("R", "load.R"))
source(here("R", "tools.R"))
source(here("R", "references.R"))
source(here("R", "analytics.R"))
source(here("R", "dependencies.R"))
source(here("R", "text_analysis.R"))
source(here("R", "sankey.R"))
source(here("R", "mfa.R"))
source(here("R", "modelling.R"))
source(here("R", "plotting.R"))
source(here("R", "supplementary_plots.R"))
source(here("R", "other_plots.R"))
source(here("R", "plot_utils.R"))
source(here("R", "theme.R"))
source(here("R", "figures.R"))
source(here("R", "output.R"))

#==============================================================================#
# ---- OPTIONS ----
#==============================================================================#

tar_option_set(
    packages = "magrittr"
)

#==============================================================================#
# ---- PIPELINE ----
#==============================================================================#

##====================================================================##
## ---- Website analytics ----
##====================================================================##

# This should be set to FALSE unless you have access to Google Analytics
include_analytics <- TRUE

if (include_analytics) {
    rlang::warn(paste(
        "Including website analytics.",
        "This will fail unless you have access to Google Analytics",
        "and have set up authentication.",
        "Set the `include_analytics` variable to FALSE in `_targets.R`",
        "to turn this off."
    ))

    website_analytics <- list(
        tar_target(
            ga_users,
            {
                googleAnalyticsR::ga_auth()
                load_ga_users(date)
            },
            packages = "googleAnalyticsR"
        ),
        tar_target(
            ga_countries,
            {
                googleAnalyticsR::ga_auth()
                load_ga_countries(date)
            },
            packages = "googleAnalyticsR"
        ),
        tar_target(
            users_plot,
            plot_users(ga_users)
        ),
        tar_target(
            users_map,
            plot_users_map(ga_countries)
        ),
        tar_target(
            save_ga_data,
            save_data_tables(
                ga_users     = ga_users,
                ga_countries = ga_countries,
                dir          = here("output", "analytics-data"),
                strict       = TRUE,
                clear        = TRUE
            ),
            format = "file"
        )
    )
} else {
    website_analytics <- NULL
}

list(
    ##====================================================================##
    ## ---- Dependencies ----
    ##====================================================================##
    tar_target(
        date,
        "2021-09-26"
    ),
    tar_target(
        johnnydep_path,
        {
            path <- Sys.getenv("JOHNNYDEP_PATH")
            if (path == "") {
                stop("johnnydep path not found. See README.")
            }
            path
        }
    ),
    ##====================================================================##
    ## ---- GitHub SHAs ----
    ##====================================================================##
    tar_target(
        tools_sha,
        get_path_sha("database/tools.tsv", date = date),
    ),
    tar_target(
        category_descs_sha,
        get_path_sha("database/categories.tsv", date = date),
    ),
    tar_target(
        categories_idx_sha,
        get_path_sha("database/categories-idx.tsv", date = date),
    ),
    tar_target(
        references_sha,
        get_path_sha("database/references.tsv", date = date),
    ),
    tar_target(
        repositories_sha,
        get_path_sha("database/repositories.tsv", date = date),
    ),
    tar_target(
        doi_idx_sha,
        get_path_sha("database/doi-idx.tsv", date = date),
    ),
    tar_target(
        ref_links_sha,
        get_path_sha("database/reference-links.tsv", date = date),
    ),
    ##====================================================================##
    ## ---- Loading ----
    ##====================================================================##
    tar_target(
        tools_raw,
        load_tools_sha(tools_sha)
    ),
    tar_target(
        category_descs,
        load_category_descs_sha(category_descs_sha)
    ),
    tar_target(
        categories_idx,
        load_categories_idx_sha(categories_idx_sha)
    ),
    tar_target(
        categories,
        categories_idx %>%
            dplyr::mutate(Present = TRUE) %>%
            tidyr::complete(Tool, Category, fill = list(Present = FALSE)) %>%
            tidyr::pivot_wider(names_from = Category, values_from = Present) %>%
            setNames(paste0("Cat", names(.))) %>%
            dplyr::rename(Tool = CatTool)
    ),
    tar_target(
        references,
        load_references_sha(references_sha)
    ),
    tar_target(
        doi_idx,
        load_doi_idx_sha(doi_idx_sha)
    ),
    tar_target(
        ref_links,
        load_ref_links_sha(ref_links_sha)
    ),
    tar_target(
        repositories,
        load_repositories_sha(repositories_sha)
    ),
    tar_target(
        gh_repos,
        load_github_repositories(repositories)
    ),
    tar_target(
        tools,
        augment_tools(tools_raw, references, doi_idx, repositories, gh_repos)
    ),
    tar_target(
        bioc_pkgs,
        {
            repositories %>%
                dplyr::filter(!is.na(Bioc)) %>%
                dplyr::pull(Bioc) %>%
                unique()
        }
    ),
    tar_target(
        cran_pkgs,
        {
            repositories %>%
                dplyr::filter(!is.na(CRAN)) %>%
                dplyr::pull(CRAN) %>%
                unique()
        }
    ),
    tar_target(
        pypi_pkgs,
        {
            repositories %>%
                dplyr::filter(!is.na(PyPI)) %>%
                dplyr::pull(PyPI) %>%
                unique()
        }
    ),
    tar_target(
        r_dependencies,
        get_r_deps(bioc_pkgs, cran_pkgs)
    ),
    tar_target(
        pypi_dependencies,
        get_pypi_deps(pypi_pkgs, johnnydep_path)
    ),
    tar_target(
        sc_stopwords,
        get_sc_stopwords()
    ),
    tar_target(
        top_words,
        get_top_words()
    ),
    ##====================================================================##
    ## ---- Modelling ----
    ##====================================================================##
    tar_target(
        mfa_variables,
        get_mfa_variables()
    ),
    tar_target(
        mfa_data,
        tools %>%
            dplyr::left_join(categories, by = "Tool") %>%
            dplyr::select(
                Tool,
                dplyr::any_of(
                    unlist(
                        purrr::map(mfa_variables, ~ .x$variables),
                        use.names = FALSE
                    )
                )
            ) %>%
            dplyr::mutate(
                dplyr::across(
                    tidyselect:::where(is.logical),
                    as.factor
                )
            ) %>%
            tibble::column_to_rownames("Tool")
    ),
    tar_target(
        mfa,
        run_mfa(mfa_data, mfa_variables)
    ),
    tar_target(
        hcpc,
        FactoMineR::HCPC(
            mfa,
            nb.clust = 8,
            consol   = TRUE,
            graph    = FALSE
        )
    ),
    tar_target(
        umap,
        uwot::umap(mfa$ind$coord, min_dist = 1)
    ),
    tar_target(
        publications_models,
        model_publications(references, ref_links)
    ),
    tar_target(
        publications_models_df,
        tidy_models(
            publications_models,
            types = c(
                citations = "Citations",
                altmetric = "Altmetric"
            ),
            term_labels = c(
                "(Intercept)"                 = "(Intercept)",
                "log2(NumReferences + 1)"     = "log2(Num references + 1)",
                "log2(NumAuthors)"            = "log2(Num authors)",
                "HasPreprintTRUE"             = "Has preprint",
                "splines::ns(Years, df = 3)3" = "Years (3rd degree)",
                "splines::ns(Years, df = 3)2" = "Years (2nd degree)",
                "splines::ns(Years, df = 3)1" = "Years (1st degree)"
            )
        )
    ),
    tar_target(
        publications_models_fits,
        tidy_models_fits(
            publications_models,
            types = c(
                citations = "Citations",
                altmetric = "Altmetric"
            )
        )
    ),
    tar_target(
        tools_models,
        model_tools(tools)
    ),
    tar_target(
        tools_models_df,
        tidy_models(
            tools_models,
            types = c(
                citations = "Total citations",
                altmetric = "Total altmetric",
                gh_stars  = "GitHub stars"
            ),
            term_labels = c(
                "(Intercept)"                      = "(Intercept)",
                "GHContributors"                   = "GitHub contributors",
                "HasRepoTRUE"                      = "Has repository",
                "HasLicenseTRUE"                   = "Has license",
                "PlatformR"                        = "Platform (R)",
                "PlatformPython"                   = "Platform (Python)",
                "PlatformBoth"                     = "Platform (Both)",
                "Preprints"                        = "Preprints",
                "Publications"                     = "Publications",
                "splines::ns(GHAgeYears, df = 3)3" = "Years (3rd degree)",
                "splines::ns(GHAgeYears, df = 3)2" = "Years (2nd degree)",
                "splines::ns(GHAgeYears, df = 3)1" = "Years (1st degree)"
            )
        )
    ),
    tar_target(
        tools_models_fits,
        tidy_models_fits(
            tools_models,
            types = c(
                citations = "Total citations",
                altmetric = "Total altmetric",
                gh_stars  = "GitHub stars"
            )
        )
    ),
    website_analytics,
    ##====================================================================##
    ## ---- Plotting ----
    ##====================================================================##
    tar_target(
        platforms_bar_plot,
        plot_platforms_bar(tools)
    ),
    tar_target(
        licenses_bar_plot,
        plot_licenses_bar(tools)
    ),
    tar_target(
        repositories_bar_plot,
        plot_repositories_bar(tools)
    ),
    tar_target(
        categories_bar_plot,
        plot_categories_bar(categories_idx, category_descs)
    ),
    tar_target(
        tools_plot,
        plot_tools_over_time(tools)
    ),
    tar_target(
        platforms_time_plot,
        plot_platforms_over_time(tools)
    ),
    tar_target(
        delay_plot,
        plot_publication_delay(ref_links, references)
    ),
    tar_target(
        pub_status_plot,
        plot_publication_status(tools)
    ),
    tar_target(
        gh_stats_plot,
        plot_gh_stats(gh_repos)
    ),
    tar_target(
        category_trend_plot,
        plot_category_prop_trend(categories, tools)
    ),
    tar_target(
        publications_models_plot,
        plot_models(publications_models_df, publications_models_fits)
    ),
    tar_target(
        tools_models_plot,
        plot_models(tools_models_df, tools_models_fits)
    ),
    tar_target(
        word_trends_plot,
        plot_words_trend(references, sc_stopwords, top_words)
    ),
    tar_target(
        wordclouds_plot,
        plot_wordclouds(references, sc_stopwords)
    ),
    tar_target(
        linked_prop_bar,
        plot_linked_prop(references, ref_links)
    ),
    ##====================================================================##
    ## ---- Supplementary plots ----
    ##====================================================================##
    tar_target(
        dependencies_plot,
        plot_dependencies(r_dependencies, pypi_dependencies)
    ),
    tar_target(
        dependencies_png,
        ggplot2::ggsave(
            plot     = dependencies_plot,
            filename = here("output", "supplementary", "dependencies.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 24,
            units    = "cm",
            res      = 300,
            scaling  = 1,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        categories_platforms_plot,
        plot_categories_platforms(categories_idx, tools)
    ),
    tar_target(
        categories_platforms_png,
        ggplot2::ggsave(
            plot     = categories_platforms_plot,
            filename = here(
                "output", "supplementary", "categories_platforms.png"
            ),
            device   = ragg::agg_png,
            width    = 16,
            height   = 20,
            units    = "cm",
            res      = 300,
            scaling  = 0.8,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        categories_per_tool_plot,
        plot_categories_per_tool(categories_idx, tools)
    ),
    tar_target(
        categories_per_tool_png,
        ggplot2::ggsave(
            plot     = categories_per_tool_plot,
            filename = here(
                "output", "supplementary", "categories_per_tool.png"
            ),
            device   = ragg::agg_png,
            width    = 20,
            height   = 14,
            units    = "cm",
            res      = 300,
            scaling  = 0.8,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        add_delay_plot,
        plot_add_delay(tools, references, doi_idx)
    ),
    tar_target(
        add_delay_png,
        ggplot2::ggsave(
            plot     = add_delay_plot,
            filename = here(
                "output", "supplementary", "add_delay.png"
            ),
            device   = ragg::agg_png,
            width    = 20,
            height   = 12,
            units    = "cm",
            res      = 300,
            scaling  = 0.8,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        correlations_plot,
        plot_metric_correlations(references, tools)
    ),
    tar_target(
        correlations_png,
        ggplot2::ggsave(
            plot     = correlations_plot,
            filename = here(
                "output", "supplementary", "metric_correlations.png"
            ),
            device   = ragg::agg_png,
            width    = 20,
            height   = 24,
            units    = "cm",
            res      = 300,
            scaling  = 0.8,
            bg       = "white"
        ),
        format = "file"
    ),
    ##====================================================================##
    ## ---- Other plots ----
    ##====================================================================##
    tar_target(
        sankey,
        plot_sankey(
            data   = get_sankey_data(tools),
            value  = "Count",
            colour = "Platform",
            width  = 0.2,
            space  = 0.05
        )
    ),
    tar_target(
        landscape_umap,
        plot_landscape_umap(mfa_data, hcpc, umap)
    ),
    tar_target(
        category_props_plot,
        plot_category_props(categories, tools)
    ),
    tar_target(
        category_barcodes_plot,
        plot_category_barcodes(categories, tools)
    ),
    ##====================================================================##
    ## ---- Figures ----
    ##====================================================================##
    tar_target(
        overview_figure,
        make_overview_figure(
            platforms_bar_plot,
            repositories_bar_plot,
            licenses_bar_plot,
            tools_plot,
            pub_status_plot,
            categories_bar_plot
        )
    ),
    tar_target(
        overview_figure_png,
        ggplot2::ggsave(
            plot     = overview_figure,
            filename = here("output", "figures", "overview.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 14,
            units    = "cm",
            res      = 300,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        overview_figure_pdf,
        ggplot2::ggsave(
            plot     = overview_figure,
            filename = here::here("output", "figures", "overview.pdf"),
            device   = grDevices::cairo_pdf,
            width    = 20,
            height   = 14,
            units    = "cm",
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        trends_figure,
        make_trends_figure(
            platforms_time_plot,
            category_trend_plot,
            word_trends_plot,
            wordclouds_plot
        )
    ),
    tar_target(
        trends_figure_png,
        ggplot2::ggsave(
            plot     = trends_figure,
            filename = here("output", "figures", "trends.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 16,
            units    = "cm",
            res      = 300,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        trends_figure_pdf,
        ggplot2::ggsave(
            plot     = trends_figure,
            filename = here::here("output", "figures", "trends.pdf"),
            device   = grDevices::cairo_pdf,
            width    = 20,
            height   = 16,
            units    = "cm",
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        open_figure,
        make_open_figure(
            linked_prop_bar,
            delay_plot,
            gh_stats_plot,
            publications_models_plot,
            tools_models_plot
        )
    ),
    tar_target(
        open_figure_png,
        ggplot2::ggsave(
            plot     = open_figure,
            filename = here("output", "figures", "open.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 18,
            units    = "cm",
            res      = 300,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        open_figure_pdf,
        ggplot2::ggsave(
            plot     = open_figure,
            filename = here::here("output", "figures", "open.pdf"),
            device   = grDevices::cairo_pdf,
            width    = 20,
            height   = 18,
            units    = "cm",
            bg       = "white"
        ),
        format = "file"
    ),
    ##====================================================================##
    ## ---- Output ----
    ##====================================================================##
    tar_target(
        save_data,
        save_data_tables(
            category_descs      = category_descs,
            categories_idx      = categories_idx,
            categories          = categories,
            references          = references,
            doi_idx             = doi_idx,
            ref_links           = ref_links,
            repositories        = repositories,
            gh_repos            = gh_repos,
            tools               = tools,
            r_dependencies      = r_dependencies,
            pypi_dependencies   = pypi_dependencies,
            publications_models = publications_models_df,
            publications_fits   = publications_models_fits,
            tools_models        = tools_models_df,
            tools_fits          = tools_models_fits,
            dir                 = here("output", "data-tables"),
            strict              = TRUE,
            clear               = TRUE
        ),
        format = "file"
    ),
    tar_target(
        publications_models_tsv,
        save_model_coefficients(
            publications_models_df,
            here("output", "tables", "publications-models.tsv")
        ),
        format = "file"
    ),
    tar_target(
        tools_models_tsv,
        save_model_coefficients(
            tools_models_df,
            here("output", "tables", "tools-models.tsv")
        ),
        format = "file"
    )
)
