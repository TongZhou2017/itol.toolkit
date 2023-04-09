[![CRAN status](https://www.r-pkg.org/badges/version/itol.toolkit)](https://CRAN.R-project.org/package=itol.toolkit)

<img src="man/figures/itol.toolkit_logo.jpg" width="480"/>

# itol.toolkit

The itol.toolkit is an R package that provides helper functions for the [Interactive Tree Of Life (iTOL)](https://itol.embl.de/). This package has been selected as a third-party tool in [iTOL documentation](https://itol.embl.de/help.cgi#external) and is recommended as one of the [Top 40 New CRAN packages in January 2023](https://rviews.rstudio.com/2023/02/28/january-2023-top-40-new-cran-packages/) by the R Views channel of RStudio.

## Features

-   Support all 114 themes among all 23 template types in iTOL v6

-   High throughput generate templates in one command

-   Learn published template themes and use theme

-   Save all-in-one reproducible data locally

## Installation

Based on the dependence packages from CRAN and Bioconductor source. We recommend to use `pak` to install `itol.toolkit` package automatically to avoid problems.

``` r
install.packages("pak")

# from CRAN
pak::pak('itol.toolkit')

# from GitHub
pak::pak('TongZhou2017/itol.toolkit')
```

If you prefer not to use the `pak` method, you can still use the traditional installation method.<details><summary>[Click to view] Traditional method</summary> To install the stable versions, you can use the CRAN official repository. For development versions, you can use the GitHub repository. However, if you need to install packages from Bioconductor, you'll need to use the BiocManager package. 

``` r
# install Biostrings
# install.packages("BiocManager")
BiocManager::install("Biostrings")

# from CRAN
install.packages("itol.toolkit")

# from GitHub
# install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("TongZhou2017/itol.toolkit")
```

Please note that in order to use this software, you will need to manually install the required dependencies from Bioconductor. A complete list of the necessary packages and installation instructions can be found in the [supplementary materials](https://tongzhou2017.github.io/itol.toolkit/articles/Installation.html#problems-caused-by-dependency-packages).</details>

 

If you encounter any issues during the installation process, such as problems caused by other systems, R versions, or dependency packages, please refer to the [supplementary materials](https://tongzhou2017.github.io/itol.toolkit/articles/Installation.html) for a solution.

## Quickstart

``` r
# load package
library(itol.toolkit)

# read data
tree <- system.file("extdata",
                    "tree_of_itol_templates.tree",
                    package = "itol.toolkit")
data("template_groups")
df_group <- data.frame(id = unique(template_groups$group), 
                       data = unique(template_groups$group))

# create hub
hub <- create_hub(tree = tree)

## create unit
unit <- create_unit(data = df_group, 
                    key = "Quickstart", 
                    type = "DATASET_COLORSTRIP", 
                    tree = tree)

## add unit into hub
hub <- hub + unit

## write template file
write_hub(hub,getwd())
```

## Documents

We have documents for every single function and some important tips for users:

### Single functions (Updating 20+)

- [COLLAPSE](https://tongzhou2017.github.io/itol.toolkit/articles/COLLAPSE.html): collapse branches by range id or node id.

### Tips (Updating 5+)

- [Datasets](https://tongzhou2017.github.io/itol.toolkit/articles/Datasets.html): overview of 5 example datasets for demo.

## Support

Please [open an issue](https://github.com/TongZhou2017/itol.toolkit/issues) to report bugs, propose new functions, or ask for help.

## License

[MIT License](https://github.com/TongZhou2017/itol.toolkit/blob/master/LICENSE.md)
