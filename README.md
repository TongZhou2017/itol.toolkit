[![CRAN status](https://www.r-pkg.org/badges/version/itol.toolkit)](https://CRAN.R-project.org/package=itol.toolkit)

<img src="man/figures/itol.toolkit_logo.jpg" width="480"/>

# itol.toolkit

Helper Functions for Interactive Tree Of Life (iTOL)

## Features

-   Support all 114 themes among all 23 template types in iTOL v6

-   High throughput generate templates in one command

-   Learn published template themes and use theme

-   Save all-in-one reproducible data locally

## Installation

Based on the dependence packages from CRAN and BioConductor source. We recommend to use `pak` to install `itol.package` automatically to avoid problems.

```{r}
install.packages("pak")

# from CRAN
pak::pak('itol.toolkit')

# from GitHub
pak::pak('TongZhou2017/itol.toolkit')
```

If you do not want to install `pak` package, you can use the most traditional installation method. However, pay attention to the installation of dependency packages. For a complete list of dependency packages and how to install them, see supplementary materials.

There are two options available:

Stable versions can be installed from CRAN official or development versions can be installed from GitHub, but dependency packages from BioConductor need to be installed using BiocManager.

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

If you have installation problems caused by other systems, R versions, or dependency packages, go to [supplementary materials](https://tongzhou2017.github.io/itol.toolkit/articles/Installation.html) for a solution.

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

## Support

Please [open an issue](https://github.com/TongZhou2017/itol.toolkit/issues) to report bugs, propose new functions, or ask for help.

## License

[MIT License](https://github.com/TongZhou2017/itol.toolkit/blob/master/LICENSE.md)
