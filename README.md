[![CRAN status](https://www.r-pkg.org/badges/version/itol.toolkit)](https://CRAN.R-project.org/package=itol.toolkit)

<img src="man/figures/itol.toolkit.gif" width="160"/>

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

### Single functions

- [COLLAPSE](https://tongzhou2017.github.io/itol.toolkit/articles/COLLAPSE.html): collapse branches by range id or node id.

- [PRUNE](https://tongzhou2017.github.io/itol.toolkit/articles/PRUNE.html): drop out branches.

- [SPACING](https://tongzhou2017.github.io/itol.toolkit/articles/SPACING.html): adjust branch spacing.

- [TREE_COLORS](https://tongzhou2017.github.io/itol.toolkit/articles/TREE_COLORS.html): set branch style at range, clade, branch, label, and background level.

- [DATASET_STYLE](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_STYLE.html): set branch style at branch and label level.

- [LABELS](https://tongzhou2017.github.io/itol.toolkit/articles/LABELS.html): change node name.

- [DATASET_TEXT](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_TEXT.html): add text by HTML.

- [DATASET_COLORSTRIP](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_COLORSTRIP.html): add color strip with text.

- [DATASET_BINARY](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_BINARY.html): multi columns with one shape symbol.

- [DATASET_GRADIENT](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_GRADIENT.html): one column heatmap.

- [DATASET_HEATMAP](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_HEATMAP.html): heatmap with field tree.

- [DATASET_SYMBOL](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_SYMBOL.html): internal tree one column with multi shape symbol.

- [DATASET_EXTERNALSHAPE](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_EXTERNALSHAPE.html): outside multi column with multi shape symbol.

- [DATASET_DOMAINS](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_DOMAINS.html): strucutre with multi shape symbol.

- [DATASET_SIMPLEBAR](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_SIMPLEBAR.html): bar plot.

- [DATASET_MULTIBAR](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_MULTIBAR.html): multi bar plot.

- [DATASET_BOXPLOT](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_BOXPLOT.html): box plot.

- [DATASET_LINECHART](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_LINECHART.html): line plot.

- [DATASET_PIECHART](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_PIECHART.html): pie plot.

- [DATASET_ALIGNMENT](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_ALIGNMENT.html): sequence alignment.

- [DATASET_CONNECTIONS](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_CONNECTIONS.html): network between braches.

- [DATASET_IMAGE](https://tongzhou2017.github.io/itol.toolkit/articles/DATASET_IMAGE.html): add image.

- [POPUP_INFO](https://tongzhou2017.github.io/itol.toolkit/articles/POPUP_INFO.html): interactive external information.

### Tips

- [Overview](https://tongzhou2017.github.io/itol.toolkit/articles/OVERVIEW_TREE_STRUCTURE.html): overview for all functions.

- [Installation](https://tongzhou2017.github.io/itol.toolkit/articles/Installation.html): FAQ about installation.

- [Datasets](https://tongzhou2017.github.io/itol.toolkit/articles/Datasets.html): overview of 5 example datasets for demo.

- [Colors](https://tongzhou2017.github.io/itol.toolkit/articles/Color_Palette.html): buildin color palette.

- [Data reproduction](https://tongzhou2017.github.io/itol.toolkit/articles/Data_Reproduction.html): learn data from template files.

- [VS table2itol](https://tongzhou2017.github.io/itol.toolkit/articles/Comparison_table2itol.html): compare with the other iTOL helper tool, table2itol.

- [Tree construction](https://tongzhou2017.github.io/itol.toolkit/articles/tree_construction.html): create a tree by different data and methods.

### Video

[![Watch the video](https://img.youtube.com/vi/aacPCzLi404/maxresdefault.jpg)](https://youtu.be/aacPCzLi404)

## Gallery

We collected reproducible plots into a [gallery](https://tongzhou2017.github.io/itol.toolkit/articles/Image_Gallery.html) page.

## Support

Please [open an issue](https://github.com/TongZhou2017/itol.toolkit/issues) to report bugs, propose new functions, or ask for help.

## License

[MIT License](https://github.com/TongZhou2017/itol.toolkit/blob/master/LICENSE.md)
