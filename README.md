[![CRAN status](https://www.r-pkg.org/badges/version/itol.toolkit)](https://CRAN.R-project.org/package=itol.toolkit)

<img src="man/figures/itol.toolkit.gif" width="160"/>

The itol.toolkit is an R package that provides helper functions for the [Interactive Tree Of Life (iTOL)](https://itol.embl.de/). This package has been selected as a third-party tool in [iTOL documentation](https://itol.embl.de/help.cgi#external) and is recommended as one of the [Top 40 New CRAN packages in January 2023](https://rviews.rstudio.com/2023/02/28/january-2023-top-40-new-cran-packages/) by the R Views channel of RStudio.

First version published in [Bioinformatics](https://doi.org/10.1093/bioinformatics/btad339) journal, Please cite:

Zhou, T., Xu, K., Zhao, F., Liu, W., Li, L., Hua, Z., & Zhou, X. (2023). itol. toolkit accelerates working with iTOL (Interactive Tree of Life) by an automated generation of annotation files. Bioinformatics, 39(6), btad339. https://doi.org/10.1093/bioinformatics/btad339


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

We have documents for every single function and some important tips for users. 

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

We collected reproducible plots into a [gallery](https://tongzhou2017.github.io/itol.toolkit-gallery/) page.

## News

update history:

Version 1.1.10

- **Added:** Correcting the spelling of the `type` parameter.

- **Added:** Beautifying the loading prompt format.

- **Fixed:** Fixing the SHOW_VALUE output parameter for the simplebar unit. 


Version 1.1.9

- **Fixed:** Meaningless warning in the `simplebar` template.

- **Fixed:** Clustering error when `heatmap` input contains a single column.

- **Added:** Support for simultaneous tree output of Hub.

- **Fixed:** Bug when using the `+` operation with `heatmap` units.

- **Added:** Color sorting algorithm with support for custom root colors.

- **Added:** Two tree construction methods (based on string vectors and data frame row/column names).

Version 1.1.8

- **Added:** Citation information while loading itol.toolkit package

- **Added:** Color distance calculation and gradient color generation

- **Added:** Color generation by two factors

Version 1.1.7

- **Fixed:** Remove the dependence on the Biostrings package to make the installation easier.

- **Fixed:** Debug the file_get_dir and file_get_name functions while without an uplevel path.

Version 1.1.6

- **Added:** Add search_tree_files function to search the NEWICK format files in a directory.

- **Added:** Add train_theme function to learn themes from template files in a directory.

- **Added:** Update dataset5 files.

- **Fixed:** Update inbuilt_theme data, which corrected the legend block in the COLOR_STRIP template file.

- **Fixed:** Fix the create_uint function about the COLOR_STRIP type, which makes the legend function work in the COLOR_STRIP template.

Version 1.1.5

- **Added:** Support two factor for shape and color in DATASET_DOMAIN

- **Fixed:** write_hub error in DATASET_LINECHART, while node data is empty

Version 1.1.4

- **Fixed:** write_hub error in DATASET_DOMAIN, while node data is empty

- **Fixed:** write_unit error in LABLE, while the file parameter is empty

Version 1.1.3

- **Added:** Set getwd() as default value of file parameter in write_unit function

- **Added:** Added new case of the file parameter in write_unit function: If the value is a director, then use the key value in unit@profile$name as the output file name.

Version 1.1.2

- **Fixed:** the dependence of nchar related to Rcpp package for R version lower than 4.0.0

Version 1.1.1

- **Added:** description in Get_Start doc

- **Fixed:** DATASET_EXTERNALSHAPE loss field_length error

- **Fixed:** DATASET_SYMBOL no legend

- **Fixed:** simplify key parameter description in create_unit function

Version 1.1.0

- **Added:** DATASET_STYLE, DATASET_COLORSTRIP, TREE_COLORS range, DATASET_SYMBOL, DATASET_DOMAINS, DATASET_CONNECTION, DATASET_MULTIBAR, DATASET_BINARY, DATASET_TEXT, DATASET_EXTERNALSHAPE, DATASET_PIECHART support full color palette(table2itol wesanderson npg aaas nejm lancet jama jco ucscgb d3 igv locuszoom uchicago simpsons futurama rickandmorty startrek tron gsea material BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral Accent Dark2 Paired Pastel1 Pastel2 Set1 Set2 Set3 Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd)

- **Added:** user level add DATASET_MULTIBAR, DATASET_LINECHART, DATASET_PIECHART, DATASET_ALIGNMENT, DATASET_IMAGE, POPUP_INFO support

- **Added:** create_hub supports direct input of phylo objects for the tree parameter, not just the tree file path

- **Fixed:** wesanderson color palette random picking function changed from runif to sample, fixed the problem of repeated color matching

Version 1.0.5

- **Added:** a new S4 method + for itol.unit and itol.unit. This method will merge the data from the same template method.

Version 1.0.4

- **Added:** Add wesanderson palettes in range case of DATASET_COLORSTRIP type

Version 1.0.3

- **Added:** interactive complex_html_text RStudio Addins for DATASET_TEXT

Version 1.0.2

- **Fixed:** while create_unit(type="DATASET_TEXT"), unable to use one character to setup color parameter (Unable to indentify data column)

- **Fixed:** while unit type is TREE_COLORS and node data is empty, unable to use hub_to_unit (attempt to use zero-length variable name)

- **Added:** Add wesanderson palettes in range case of TREE_COLORS type

## Support

Please [open an issue](https://github.com/TongZhou2017/itol.toolkit/issues) to report bugs, propose new functions, or ask for help.

## License

[MIT License](https://github.com/TongZhou2017/itol.toolkit/blob/master/LICENSE.md)
