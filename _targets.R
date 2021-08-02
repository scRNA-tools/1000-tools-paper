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
source(here("R", "plotting.R"))
source(here("R", "figures.R"))

#==============================================================================#
# ---- OPTIONS ----
#==============================================================================#

tar_option_set(
    packages = "magrittr"
)

#==============================================================================#
# ---- PIPELINE ----
#==============================================================================#

list(
    tar_target(
        date,
        "2021-07-31"
    ),
    tar_target(
        tools_sha,
        get_path_sha("database/tools.tsv", date = date),
        cue = tar_cue("always")
    ),
    tar_target(
        tools_raw,
        load_tools_sha(tools_sha)
    ),
    tar_target(
        categories_idx_sha,
        get_path_sha("database/categories-idx.tsv", date = date),
        cue = tar_cue("always")
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
        references_sha,
        get_path_sha("database/references.tsv", date = date),
        cue = tar_cue("always")
    ),
    tar_target(
        references,
        load_references_sha(references_sha)
    ),
    tar_target(
        repositories_sha,
        get_path_sha("database/repositories.tsv", date = date),
        cue = tar_cue("always")
    ),
    tar_target(
        doi_idx_sha,
        get_path_sha("database/doi-idx.tsv", date = date),
        cue = tar_cue("always")
    ),
    tar_target(
        doi_idx,
        load_doi_idx_sha(doi_idx_sha)
    ),
    tar_target(
        ref_links_sha,
        get_path_sha("database/reference-links.tsv", date = date),
        cue = tar_cue("always")
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
        ga_users,
        {
            googleAnalyticsR::ga_auth()
            load_ga_users(date)
        },
        packages = "googleAnalyticsR",
        cue      = tar_cue("always")
    ),
    tar_target(
        ga_countries,
        {
            googleAnalyticsR::ga_auth()
            load_ga_countries(date)
        },
        packages = "googleAnalyticsR",
        cue      = tar_cue("always")
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
        plot_categories_bar(categories_idx)
    ),
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
        landscape_umap,
        plot_landscape_umap(mfa_data, hcpc, umap)
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
        category_props_plot,
        plot_category_props(categories, tools)
    ),
    tar_target(
        category_barcodes_plot,
        plot_category_barcodes(categories, tools)
    ),
    tar_target(
        category_trend_plot,
        plot_category_prop_trend(categories, tools)
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
        dependencies_plot,
        plot_dependencies(r_dependencies, pypi_dependencies)
    ),
    tar_target(
        publications_models_plot,
        plot_publications_models(references, ref_links)
    ),
    tar_target(
        tools_models_plot,
        plot_tools_models(tools)
    ),
    tar_target(
        sc_stopwords,
        get_sc_stopwords()
    ),
    tar_target(
        top_words,
        get_top_words()
    ),
    tar_target(
        word_trends_plot,
        plot_words_trend(references, sc_stopwords, top_words)
    ),
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
            filename = here("output", "overview_figure.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 14,
            units    = "cm",
            res      = 300,
            scaling  = 0.5,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        trends_figure,
        make_trends_figure(
            platforms_time_plot,
            category_trend_plot,
            word_trends_plot
        )
    ),
    tar_target(
        trends_figure_png,
        ggplot2::ggsave(
            plot     = trends_figure,
            filename = here("output", "trends_figure.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 12,
            units    = "cm",
            res      = 300,
            scaling  = 0.5,
            bg       = "white"
        ),
        format = "file"
    ),
    tar_target(
        open_figure,
        make_open_figure(
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
            filename = here("output", "open_figure.png"),
            device   = ragg::agg_png,
            width    = 20,
            height   = 16,
            units    = "cm",
            res      = 300,
            scaling  = 0.5,
            bg       = "white"
        ),
        format = "file"
    )
)
