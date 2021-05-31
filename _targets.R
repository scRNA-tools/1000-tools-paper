#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

library(targets)

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

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
        categories_idx_sha,
        gh::gh(paste0(
            "GET /repos/scRNA-tools/scRNA-tools/commits?until=",
            date,
            "&path=/database/categories-idx.tsv"
        ))[[1]]$sha
    ),
    tar_target(
        categories_idx,
        readr::read_tsv(
            paste0(
                "https://github.com/scRNA-tools/scRNA-tools/raw/",
                categories_idx_sha,
                "/database/categories-idx.tsv"
            ),
            col_types = readr::cols(
                Tool     = readr::col_character(),
                Category = readr::col_character()
            )
        )
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
