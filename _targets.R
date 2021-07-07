#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

library(targets)
library(here)

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

source(here("R", "github.R"))
source(here("R", "load.R"))
source(here("R", "tools.R"))
source(here("R", "references.R"))
source(here("R", "analytics.R"))
source(here("R", "dependencies.R"))
source(here("R", "sankey.R"))
source(here("R", "mfa.R"))
source(here("R", "plotting.R"))

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
        plot_gh_stats(tools)
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
    )
)
