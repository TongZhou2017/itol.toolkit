#' The itol.hub Class
#'
#' @description The itol.hub object is an intermediate storage container used internally throughout the
#' integration procedure to hold bits of data that are useful downstream.
#'
#' @slot tree a list of meta data table, usually raw, full, and analyze
#' @slot seq identity of the active assay
#' @slot abundance abundance
#' @slot taxonomy taxonomy
#' @slot meta.data other meta.data
#' @slot theme itol theme
#'
#' @importFrom methods new
#' @name itol.hub-class
#' @rdname itol.hub-class
#' @concept objects
#' @exportClass itol.hub
#'
itol.hub <- setClass(
  Class = "itol.hub",
  slots = list(
    tree = "list",
    seq = "data.frame",
    abundance = "data.frame",
    taxonomy = "list",
    meta.data = "list",
    theme = "list"
  )
)

#' Create itol.hub Object
#' @description create a new object for itol.hub
#'
#' @param tree tree file
#' @param field_tree todo
#' @param seq todo
#' @param abundance todo
#' @param taxonomy todo
#' @param node_data todo
#' @param tip_data todo
#' @importFrom ape read.tree
#' @importFrom ape makeNodeLabel
#' @return Returns a itol.hub object
#'
#' @export
#' @concept object
#'
#' @examples
#' TREE <- system.file("extdata", "tree_of_itol_templates.tree", package = "itol.toolkit")
#' create_hub(tree = TREE)
#'
create_hub <- function(tree,field_tree=NULL,seq=NULL,abundance=NULL,taxonomy=NULL,node_data=NULL,tip_data=NULL) {
  tree <- list(main = read.tree(tree), field = list())
  if (any(duplicated(tree$main$node.label)) || is.null(tree$main$node.label)) {
    tree$main <- ape::makeNodeLabel(phy = tree$main, method = "number", prefix = "I")
  }
  if(!is.null(field_tree)){
    field_tree_name = file_get_name(str = field_tree, with_ext = FALSE, keep_dir = FALSE)
    tree$field <- list(new_element = read.tree(field_tree))
    names(tree$field)[length(names(tree$field))] <- field_tree_name
  }
  seq <- data.frame(id = tree$main$tip.label)
  abundance <- data.frame(id = tree$main$tip.label)
  taxonomy <- list(node = data.frame(id = tree$main$node.label), tip = data.frame(id = tree$main$tip.label))
  meta.data <- list(node = data.frame(id = tree$main$node.label), tip = data.frame(id = tree$main$tip.label))
  theme <- list()
  object <- new("itol.hub", tree = tree, seq = seq, abundance = abundance, taxonomy = taxonomy, meta.data = meta.data, theme = theme)
  return(object)
}

# S4 method
#' show method for S4 class itol.hub
#' @param object An object of class itol.hub
#' @return a stdout screen information about itol.hub object
#' @importMethodsFrom methods show
#' @export
setMethod(
  "show",
  "itol.hub",
  function(object) {
    cat("An object of class \"itol.hub\" from package itol.toolkit\n")
    cat("Main tree: ", length(object@tree$main$tip.label), " tips\n")
    cat("Feild tree: ", length(object@tree$field), " trees\n")
    cat("Sequence: ", dim(object@seq)[2] - 1, " datasets\n")
    cat("Abundance: ", length(unique(stringr::str_remove(names(object@abundance)[grep("\\$", names(object@abundance))], "\\$.*$"))), " datasets\n")
    cat("Taxonomy: ", dim(object@taxonomy$tip)[2] - 1, " levels\n")
    cat("Node data: ", length(unique(stringr::str_remove(names(object@meta.data$node)[grep("\\$", names(object@meta.data$node))], "\\$.*$"))), " datasets\n")
    cat("Tip data: ", length(unique(stringr::str_remove(names(object@meta.data$tip)[grep("\\$", names(object@meta.data$tip))], "\\$.*$"))), " datasets\n")
    cat("Themes: ", length(object@theme), "\n")
  }
)

#' The itol.unit Class
#'
#' @description The itol.unit object is an intermediate storage container used internally throughout the
#' integration procedure to hold bits of data that are useful downstream.
#'
#' @slot type a list of meta data table, usually raw, full, and analyze
#' @slot sep identity of the active assay
#' @slot profile abundance
#' @slot field taxonomy
#' @slot common_themes other meta.data
#' @slot specific_themes itol theme
#' @slot data data
#'
#' @importFrom methods new
#' @name itol.unit-class
#' @rdname itol.unit-class
#' @concept objects
#' @exportClass itol.unit
#'
itol.unit <- setClass(
  Class = "itol.unit",
  slots = list(
    type = "character",
    sep = "character",
    profile = "list",
    field = "list",
    common_themes = "list",
    specific_themes = "list",
    data = "list"
  )
)


#' Create itol.unit Object from file
#' @description create a new object for itol.unit
#'
#' @param file template file
#' @param tree tree file
#' @param ... Further arguments to be passed to subsequent functions.
#' @import dplyr
#' @importFrom purrr discard
#' @importFrom methods new
#' @return Returns a itol.unit object
#'
#' @export
#' @concept object

file_to_unit <- function(file, tree, ...) {
  file_name <- file_get_name(str = file, with_ext = F, keep_dir = F)
  file_name <- sub(" ", "_", file_name)
  type <- ""
  sep <- ""
  profile <- list()
  field <- list()
  common_themes <- list()
  specific_themes <- list()
  data <- learn_df(tree = tree, node = T, tip = T)
  lines_clean <- line_clean(file = file)
  type <- learn_type(file = file)
  sep <- learn_separator(lines_clean)
  lines_data <- line_split(lines_clean)
  if (type %in% c("COLLAPSE", "PRUNE")) {
    df_data <- learn_subdf(lines = lines_data, type = type, sep = "\t", dataset_name = file_name, ...)
  }
  if (type %in% c("SPACING", "TREE_COLORS", "LABELS", "POPUP_INFO")) {
    df_data <- learn_subdf(lines = lines_data, type = type, sep = sep, dataset_name = file_name, ...)
  }
  if (type %in% c("DATASET_STYLE", "DATASET_TEXT")) {
    profile <- learn_profile(lines_clean, sep)
    common_themes <- learn_theme_common_themes(lines_clean, sep)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = sep, dataset_name = sub(" ", "_", profile[["name"]]), ...)
  }
  if (type %in% c("DATASET_COLORSTRIP", "DATASET_SYMBOL", "DATASET_DOMAINS", "DATASET_BOXPLOT", "DATASET_LINECHART", "DATASET_ALIGNMENT", "DATASET_CONNECTION", "DATASET_IMAGE")) {
    profile <- learn_profile(lines_clean, sep)
    common_themes <- learn_theme_common_themes(lines_clean, sep)
    specific_themes <- learn_theme_specific_themes(lines_clean, sep, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = sep, dataset_name = sub(" ", "_", profile[["name"]]), ...)
  }
  if (type %in% c("DATASET_GRADIENT", "DATASET_SIMPLEBAR")) {
    profile <- learn_profile(lines_clean, sep)
    common_themes <- learn_theme_common_themes(lines_clean, sep)
    specific_themes <- learn_theme_specific_themes(lines_clean, sep, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = sep, dataset_name = sub(" ", "_", profile[["name"]]), field_labels = profile[["name"]], ...)
  }
  if (type %in% c("DATASET_BINARY", "DATASET_HEATMAP", "DATASET_EXTERNALSHAPE", "DATASET_MULTIBAR", "DATASET_PIECHART")) {
    profile <- learn_profile(lines_clean, sep)
    field <- learn_field(lines_clean, sep)
    common_themes <- learn_theme_common_themes(lines_clean, sep)
    specific_themes <- learn_theme_specific_themes(lines_clean, sep, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = sep, dataset_name = sub(" ", "_", profile[["name"]]), field_labels = field[["labels"]], ...)
  }
  df_data <- convert_range_to_node(df_data, tree)
  data[["node"]] <- df_merge(data[["node"]], df_data)
  data[["tip"]] <- df_merge(data[["tip"]], df_data)
  unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data)
  return(unit)
}

#' Create itol.unit Object from object
#' @description create a new object for itol.unit
#'
#' @param object itol.hub object
#' @param theme itol.theme object
#' @param key key id of dataset name
#' @import dplyr
#' @importFrom methods new
#' @return Returns a itol.unit object
#'
#' @export
#' @concept object

hub_to_unit <- function(object, theme, key) {
  type <- ""
  sep <- ""
  profile <- list()
  field <- list()
  common_themes <- list()
  specific_themes <- list()
  data <- learn_df(tree = object@tree$main, node = TRUE, tip = TRUE)
  # position:meta.data
  # equal row number
  if (theme@type %in% c("SPACING", "COLLAPSE", "PRUNE", "DATASET_TEXT", "DATASET_COLORSTRIP", "DATASET_BINARY", "DATASET_GRADIENT", "DATASET_EXTERNALSHAPE", "DATASET_SIMPLEBAR", "DATASET_MULTIBAR", "DATASET_BOXPLOT", "DATASET_PIECHART", "DATASET_IMAGE", "POPUP_INFO")) {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data[["node"]] <- object@meta.data$node %>% select(id, starts_with(paste0(key, "$")))
    data[["tip"]] <- object@meta.data$tip %>% select(id, starts_with(paste0(key, "$")))
  }
  # position:meta.data
  # not equal row number
  if (theme@type %in% c("TREE_COLORS", "DATASET_STYLE", "DATASET_SYMBOL", "DATASET_CONNECTION")) {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data_node <- object@meta.data$node %>% select(id, starts_with(paste0(key, "$")))
    data_tip <- object@meta.data$tip %>% select(id, starts_with(paste0(key, "$")))
    pattern <- "\\|"
    names_node <- names(data_node)
    names_tip <- names(data_tip)
    if(ncol(data_node)==1){
      data[['node']] <- data_node
    }else{
      eval(parse(text = paste0("data[['node']] <- tidyr::separate_rows(data_node,`", paste0(names_node[-1], collapse = "`,`"), "`,sep = pattern)")))
    }
    eval(parse(text = paste0("data[['tip']] <- tidyr::separate_rows(data_tip,`", paste0(names_tip[-1], collapse = "`,`"), "`,sep = pattern)")))
    data[["node"]][data[["node"]] == "NA"] <- NA
    data[["tip"]][data[["tip"]] == "NA"] <- NA
  }
  if (theme@type == "DATASET_DOMAINS") {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data_node <- object@meta.data$node %>% select(id, starts_with(paste0(key, "$")))
    data_tip <- object@meta.data$tip %>% select(id, starts_with(paste0(key, "$")))
    pattern_1 <- ","
    pattern_2 <- "\\|"
    names_node <- names(data_node)
    names_tip <- names(data_tip)
    eval(parse(text = paste0("data_node <- tidyr::separate_rows(data_node,`", paste0(key, "$DOMAINS"), "`,sep = pattern_1)")))
    eval(parse(text = paste0("data_tip <- tidyr::separate_rows(data_tip,`", key, "$DOMAINS`,sep = pattern_1)")))
    node_id <- "node"
    tip_id <- "tip"
    colnames_new <- paste0(key, "$", c("SHAPE", "START", "END", "COLOR", "LABEL"))
    eval(parse(text = paste0("data[[node_id]] <- data_node %>% tidyr::separate(`", paste0(key, "$DOMAINS"), "`, sep = pattern_2, into = colnames_new, remove = FALSE) %>% select(-`", paste0(key, "$DOMAINS"), "`)")))
    eval(parse(text = paste0("data[[tip_id]] <- data_tip %>% tidyr::separate(`", paste0(key, "$DOMAINS"), "`, sep = pattern_2, into = colnames_new, remove = FALSE) %>% select(-`", paste0(key, "$DOMAINS"), "`)")))
    data[["node"]][data[["node"]] == "NA"] <- NA
    data[["tip"]][data[["tip"]] == "NA"] <- NA
  }
  if (theme@type == "DATASET_LINECHART") {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data_node <- object@meta.data$node %>% select(id, starts_with(paste0(key, "$")))
    data_tip <- object@meta.data$tip %>% select(id, starts_with(paste0(key, "$")))
    pattern <- "\\|"
    names_node <- names(data_node)
    names_tip <- names(data_tip)
    node_id <- "node"
    tip_id <- "tip"
    eval(parse(text = paste0("data[[node_id]] <- tidyr::separate_rows(data_node,`", paste0(key, "$X"), "`,`", paste0(key, "$Y"), "`,sep = pattern)")))
    eval(parse(text = paste0("data[[tip_id]] <- tidyr::separate_rows(data_tip,`", paste0(key, "$X"), "`,`", paste0(key, "$Y"), "`,sep = pattern)")))
    data[["node"]][data[["node"]] == "NA"] <- NA
    data[["tip"]][data[["tip"]] == "NA"] <- NA
  }
  # position:taxonomy
  # equal row number
  if (theme@type == c("LABELS")) {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data[["node"]] <- object@taxonomy$node %>% select(id, starts_with(paste0(key, "$")))
    data[["tip"]] <- object@taxonomy$tip %>% select(id, starts_with(paste0(key, "$")))
  }
  # position:abundance
  # equal row number
  if (theme@type == c("DATASET_HEATMAP")) {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data[["tip"]] <- object@abundance %>% select(id, starts_with(paste0(key, "$")))
  }
  # position:seq
  # equal row number
  if (theme@type == c("DATASET_ALIGNMENT")) {
    type <- theme@type
    sep <- theme@sep
    profile <- theme@profile
    field <- theme@field
    common_themes <- theme@common_themes
    specific_themes <- theme@specific_themes
    data[["tip"]] <- object@seq %>% select(id, starts_with(paste0(key, "$")))
  }
  unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data)
  return(unit)
}

#' The itol.theme Class
#'
#' @description The itol.theme object is an intermediate storage container used internally throughout the
#' integration procedure to hold bits of data that are useful downstream.
#'
#' @slot type a list of meta data table, usually raw, full, and analyze
#' @slot sep identity of the active assay
#' @slot profile abundance
#' @slot field taxonomy
#' @slot common_themes other meta.data
#' @slot specific_themes itol theme
#'
#' @importFrom methods new
#' @name itol.theme-class
#' @rdname itol.theme-class
#' @concept objects
#' @exportClass itol.theme
#'
itol.theme <- setClass(
  Class = "itol.theme",
  slots = list(
    type = "character",
    sep = "character",
    profile = "list",
    field = "list",
    common_themes = "list",
    specific_themes = "list"
  )
)

#' Create itol.theme Object
#' @description create a new object for itol.theme
#'
#' @param unit unit object
#' @param file template file
#' @param tree tree file
#' @param ... Further arguments to be passed to subsequent functions.
#' @importFrom methods new
#' @return Returns a itol.theme object
#'
#' @export
#' @concept object

create_theme <- function(unit = NULL, file = NULL, tree = NULL, ...) {
  if (is.null(unit)) {
    unit <- file_to_unit(file = file, tree = tree)
  }
  theme <- new("itol.theme", type = unit@type, sep = unit@sep, profile = unit@profile, field = unit@field, common_themes = unit@common_themes, specific_themes = unit@specific_themes)
  return(theme)
}
