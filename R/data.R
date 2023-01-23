#' inbuilt themes
#'
#' Default themes learned from iTOL offical template examples.
#'
#' @format ## `inbuilt_themes`
#' A list with 23 template themes:
#' \describe{
#'   \item{COLLAPSE}{Default theme of collapse template}
#'   \item{PRUNE}{Default theme of prune template}
#'   \item{SPACING}{Default theme of spacing template}
#'   \item{TREE_COLORS}{Default theme of tree colors template}
#'   \item{DATASET_STYLE}{Default theme of style template}
#'   \item{LABELS}{Default theme of labels template}
#'   \item{DATASET_TEXT}{Default theme of text template}
#'   \item{DATASET_COLORSTRIP}{Default theme of colorstrip template}
#'   \item{DATASET_BINARY}{Default theme of binary template}
#'   \item{DATASET_GRADIENT}{Default theme of gradient template}
#'   \item{DATASET_HEATMAP}{Default theme of heatmap template}
#'   \item{DATASET_SYMBOL}{Default theme of symbol template}
#'   \item{DATASET_EXTERNALSHAPE}{Default theme of externalshape template}
#'   \item{DATASET_DOMAINS}{Default theme of domains template}
#'   \item{DATASET_SIMPLEBAR}{Default theme of simple bar template}
#'   \item{DATASET_MULTIBAR}{Default theme of multi bar template}
#'   \item{DATASET_BOXPLOT}{Default theme of box plot template}
#'   \item{DATASET_LINECHART}{Default theme of line chart template}
#'   \item{DATASET_PIECHART}{Default theme of pie chart template}
#'   \item{DATASET_ALIGNMENT}{Default theme of alignment template}
#'   \item{DATASET_CONNECTION}{Default theme of connection template}
#'   \item{DATASET_IMAGE}{Default theme of image template}
#'   \item{POPUP_INFO}{Default theme of popup info template}
#'   ...
#' }
"inbuilt_themes"


#' template groups
#'
#' Templates were clustered into 5 groups by parameter similarity.
#'
#' @format ## `template_groups`
#' A data frame with template group clustering reslut:
#' \describe{
#'   \item{template}{All the 23 template types of iTOL}
#'   \item{group}{5 clustring gourps:
#'   Tree structure: This group only controls the topology of tree branch
#'   merging, filtering, and spacing. There are no style and rich annotation
#'   data, even though most of the annotation data only include single-column id
#'   information and do not contain any dataset base information, sample
#'   information, or common and specific style information. It is a particularly
#'   simple type of template.
#'   Theme style: This does not change any topology or add any text information
#'   but only changes the color scheme, line type and width, and font style and
#'   size of existing information. This is an extremely comprehensive and
#'   diverse type of annotation information.
#'   Text: This group contains any templates with added text information. With
#'   super flexible and convenient annotation methods, users can modify even a
#'   single character's style in HTML. Users can also modify the text annotation
#'   style of nodes and branches in batch based on matching conditions in
#'   itol.hub objects, which require regular expression replacement and
#'   precise data filtering. This high-frequency data processing is difficult to
#'   achieve and retain the workflow in the EXCEL-based editor.
#'   Basic plot: This group contains basic visualization methods. From a
#'   functional point of view, this is the most feature-rich class of templates.
#'   The similarity of the parameters within this part is very high.The
#'   structured and uniform organization of these templates can greatly reduce
#'   code redundancy and the user workload of data organizing. Moreover,
#'   boxplot, which is not a regular enough data annotation template, can be
#'   automatedly manipulated in R. The lack of template data structure makes
#'   using frequency unbalanced among research. Hence, the frequency of using
#'   these low-frequency templates can be increased.
#'   Advanced plot: Compared with the basic visualization methods, these
#'   visualization methods contain more comprehensive data types and often
#'   require third-party tools for input data processing. But they are the most
#'   extensible type of visualization methods for iTOL.
#'   }
#'   ...
#' }
"template_groups"

#' template parameters count
#'
#' Template types and parameters count matrix. The row names are template types.
#' The column names are parameters short ids. The parameters are including the
#' themes parameters and data column names. All the details are introduced in
#' the full-page Excel file on GitHub.
#'
#' @format ## `template_parameters_count`
#' A data frame with template types and parameters 0/1 count matrix:
#' \describe{
#'   \item{V1}{head. file type head notice}
#'   \item{V2}{separator. select the separator which is used to delimit the
#'   data below (TAB,SPACE or COMMA).This separator must be used throughout this
#'   file.}
#'   \item{V3}{dataset name. label is used in the legend table}
#'   ...
#' }
"template_parameters_count"
