# 1000 tools paper

<!-- badges: start -->
[![gitmoji](https://img.shields.io/badge/gitmoji-%20😜%20😍-FFDD67.svg?style=flat-square)](https://gitmoji.dev)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

This repository contains code and analysis for the _"Over 1000 tools reveal trends in the single-cell RNA-seq analysis landscape"_ publication.

## Directory structure

* `R/` - Folder containing R code files. See documentation in files for more detail.
* `output/` - Output files created by the workflow. These versions are stored here as a record but will be overwritten by running the workflow.
  * `data-tables/` - Data files created by the workflow
  * `figures/` - Figures shown in the paper
  * `supplementary/` - Supplementary figures shown in the paper
  * `tables/` - Tables shown in the paper
* `.gitignore` - Git configuration file
* `1000-tools.Rproj` - RStudio project file
* `LICENSE.md` - License file
* `README.md` - This README file
* `renv/` - Internal **{renv}** files
* `renv.lock` - **{renv}** lock file specifying R package dependencies

### Other files created by set up or the workflow

* `.Renviron` - Local R configuration file, created as part of setting up
* `_cache/` - Cache files created by the workflow to speed up some parts of the analysis
* `_targets/` - Internal **{targets}** files

## Setting up

### R Dependencies

R package dependencies are managed using **{renv}**.
They should be automatically installed when you start an R session inside the repository but to make sure run:

```r
renv::restore()
```

### Crossref

Information about publications and preprints is retrieved from the Crossref API using the **{rcrossref}** package.
As explained in `` ?rcrossref::`rcrossref-package` `` Crossref provides faster access to people who give an email address.
To do this add the following line to your `.Renviron`:

```
crossref_email=your@email.com
```

### johnnydep

Package dependencies for PyPI tools are retrieved using the **johnnydep** tool (https://pypi.org/project/johnnydep/).
For these stages to work you must have **johnnydep** installed.
The easiest way to do that is using **pip** or **conda**:

```bash
pip install johnnydep
# OR
conda install johnnydep
```

Once **johnnydep** is installed find the path to it using `which` and set a `JOHNNYDEP_PATH` variable in your `.Renviron`.

```bash
which johnnydep
```
 
```
JOHNNYDEP_PATH=/path/to/your/johnnydep
```

### Fonts

To make sure fonts used in plots are available follow these steps:

1. Download and install the Noto Sans and Noto Sans Maths fonts
    * For MacOS users the easiest way to do this is using [Homebrew](https://brew.sh/):
    
    ```bash
    brew install font-noto-sans font-noto-sans-math
    ```
    * Noto Sans is also available from [Google Fonts](https://fonts.google.com/specimen/Noto+Sans)
2. Import fonts into R by running `extrafont::font_import()`

If these fonts are not available the plots will still be produced they will just use the standard default font.

### Analytics

This workflow can also generate analysis of usage of the scRNA-tools website but it requires access to the Google Analytics group so will need to be switched off for most people

#### No analytics access (most people)

Edit the `_targets.R` file and make sure the `include_analytics` variable is set to `FALSE`.

```r
include_analytics <- FALSE
```

#### Analytics access

Data for plots showing usage statistics of the scRNA-tools website are collected using the **{googleAnalyticsR}** website.
For this to work you must set up authentication with the `googleAnalyticsR::ga_auth_setup()` function following the instructions here https://code.markedmondson.me/googleAnalyticsR/articles/setup.html.

At the end of the process your `.Renviron` file should contain lines similar to these:

```
GAR_CLIENT_JSON=/path/to/oauth.json
GARGLE_EMAIL=your@email.com
```
## Running analysis

The analysis workflow is managed using **{targets}**.
Once set up is complete you can run the workflow using:

```r
targets::tar_make()
```

Some of the steps (collecting reference and GitHub repository information) take a while to run.
Once the workflow is complete various output files will be created in the `output/` directory.
If you want to view any of the intermediate parts of the workflow you can load the output of any target using:

```r
targets::tar_load(target_name)
```

### Updating analysis

The analysis is pinned to a particular date and version of the scRNA-tools database.
If you want to repeat the analysis for a more recent version edit `_targets.R` and
modify the `date` target:

```r
tar_target(
    date,
    "YYYY-MM-DD"
)
```

## License

The code is available under the [MIT](https://github.com/scRNA-tools/1000-tools/blob/master/LICENSE.md) license.
