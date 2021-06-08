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
        repositories,
        load_repositories_sha(repositories_sha)
    ),
    tar_target(
        gh_repos,
        load_github_repositories(repositories)
    ),
    tar_target(
        tools,
        augment_tools(tools_raw, references, doi_idx)
    ),
    tar_target(
        categories_mat,
        categories_idx %>%
            dplyr::mutate(Present = 1) %>%
            tidyr::complete(Tool, Category, fill = list(Present = 0)) %>%
            tidyr::pivot_wider(names_from = Category, values_from = Present) %>%
            tibble::column_to_rownames("Tool") %>%
            as.matrix()
    )
)
