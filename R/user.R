#' Extract theme from inbuilt_themes
#'
#' @description Extract theme from 23 template types in inbuilt_themes data
#' in package.
#' @param type a character specifying the template type used for extracting.
#' Following choices are possible: "COLLAPSE","PRUNE","SPACING","TREE_COLORS",
#' "DATASET_STYLE","LABELS","DATASET_TEXT","DATASET_COLORSTRIP",
#' "DATASET_BINARY","DATASET_GRADIENT","DATASET_HEATMAP","DATASET_SYMBOL",
#' "DATASET_EXTERNALSHAPE","DATASET_DOMAINS","DATASET_SIMPLEBAR",
#' "DATASET_MULTIBAR","DATASET_BOXPLOT","DATASET_LINECHART","DATASET_PIECHART",
#' "DATASET_ALIGNMENT","DATASET_CONNECTION","DATASET_IMAGE","POPUP_INFO.
#' @param style a character specifying the specific version of template type
#' used for extracting. The default value is "default" style for all types.
#' @return a itol.theme object containing
#' @return \item{type}{This group holds information about the template type of
#' the data only. This is a very critical piece of information. In many
#' functions of the itol.toolkit package, the template type information is
#' used to determine the different data processing and input/output methods.}
#' @return \item{sep}{This group holds data separator information only.
#' This is one of the most important parameters for data reading and output.
#' It is a separate category because it is frequently used and is an input
#' parameter for other subsequent parameters to be read.}
#' @return \item{profile}{This group contains basic information about the
#' dataset, such as the dataset name and a color label to distinguish the
#' dataset. The dataset name is extremely important. This parameter is used
#' almost throughout the data processing of the itol.toolkit package.
#' With the content of this parameter as the key value, the data and theme
#' information of the dataset are associated. In turn, high throughput
#' learning and writing of large-scale data can be achieved. This parameter
#' is not included in some template types with a particularly simple structure,
#' so we choose a file name or a user-defined method as the key value.}
#' @return \item{field}{This group contains information about each sample
#' within the dataset, and this type of parameter exists only for multi-sample
#' data. This information even includes the clustering tree between samples.
#' This information is usually stored as part of the column names in the
#' metadata part or abundance information of the itol.hub object.}
#' @return \item{common_themes}{These themes are used at high frequency in
#' different templates. These parameters are small in number but constitute some
#' common features of iTOL visual style settings, such as legend, margin, etc.}
#' @return \item{specific_themes}{These themes are used only in specific
#' templates. The number of these parameters is very large. However, most of
#' them are used in only one template to control the style details of the
#' visualization. By unifying these parameters and calling them according to the
#' template type, users can perform secondary development and data processing
#' with a high degree of parameter aggregation without worrying too much about
#' the differences between different template types.}
#' @export
#' @examples
#' theme <- use.theme("COLLAPSE")
use.theme <- function(type,style="default"){
  if (!type %in% names(inbuilt_themes)) {
    stop("Unsupported type")
  }
  if (!style %in% c("default")) {
    stop("Unsupported style")
  }
  theme <- inbuilt_themes[[type]][[style]]
  return(theme)
}

#' Create itol.unit
#' @description Create itol.unit from simple input in R environment.
#' @param data if type == "COLLAPSE", a vector of characters specifying the tips
#' or node used for collapsing used for extracting.
#' @param key a character specifying the output file name for hub object.
#' @param type a character specifying the template type used for extracting.
#' Following choices are possible: "COLLAPSE","PRUNE","SPACING","TREE_COLORS",
#' "DATASET_STYLE","LABELS","DATASET_TEXT","DATASET_COLORSTRIP",
#' "DATASET_BINARY","DATASET_GRADIENT","DATASET_HEATMAP","DATASET_SYMBOL",
#' "DATASET_EXTERNALSHAPE","DATASET_DOMAINS","DATASET_SIMPLEBAR",
#' "DATASET_MULTIBAR","DATASET_BOXPLOT","DATASET_LINECHART","DATASET_PIECHART",
#' "DATASET_ALIGNMENT","DATASET_CONNECTION","DATASET_IMAGE","POPUP_INFO.
#' @param style a character specifying the specific version of template type
#' used for extracting. The default value is "default" style for all types.
#' @param subtype a character specifying the subtype under type. If the type is
#' "TREE_COLORS", the following choices are possible: "range", "clade",
#' "branch", "label", "label_background".
#' @param color a character specifying the color pattern name. The following
#' choices are possible: "table2itol", "RColorBrewer", "ggsci".
#' @param line_type a character specifying the normal or dashed line type used
#' in clade and branch subtype.
#' @param font_type a character specifying the bold, italic, and bold-italic
#' font type used in label and branch subtype.
#' @param size_factor a number specifying the line width used in clade and
#' branch subtype and size factor in label subtype.
#' @param position If type == "DATASET_STYLE", a character
#' specifying the position: The following choices are possible: "node" and
#' "clade". If type == "DATASET_TEXT", a number specifying the position of the
#' text on the tree: -1 = external label; a number between 0 and 1 = internal
#' label positioned at the specified value along the node branch (for example,
#' position 0 is exactly at the start of node branch, position 0.5 is in the
#' middle, and position 1 is at the end)
#' @param background_color Only used while type == "DATASET_STYLE" and subtype
#' == "label". a character or a vector of character specifying the background
#' color in hexadecimal, RGB or RGBA notation.
#' @param rotation Only used while type == "DATASET_TEXT". a number or a
#' vector of number specifying the rotation angle of the text.
#' @param method a character specifying the numeric data summarise method. If
#' type == "DATASET_BINARY", the following choices are possible: "mean",
#' "sum", "binary" (for automatic binary conversion).
#' @param shape a character or a vector of character specifying the symbol
#' shape. If type == "DATASET_BINARY", the default is 2. If type ==
#' "DATASET_SYMBOL", the following choices are possible: 1 for rectangle, 2
#' for circle, 3 for star, 4 for left pointing triangle, 5 for right pointing
#' triangle. If using NULL and there are data column, the functions will
#' automaticly help users to setup the shapes based on the levels of the
#' data.
#' @param fill If type == "DATASET_SYMBOL", 1/0 is specifying the shape
#' outlier filled or not. If type == "DATASET_DOMAINS", the following choices
#' are possible: "RE|HH|HV|EL|DI|TR|TL|PL|PR|PU|PD|OC|GP".
#' @param tree a character specifying Newick format tree file path or a phylo
#' object of main phylogenetic tree.
#' @return a itol.unit object containing
#' @return \item{type}{This group holds information about the template type of
#' the data only. This is a very critical piece of information. In many
#' functions of the itol.toolkit package, the template type information is
#' used to determine the different data processing and input/output methods.}
#' @return \item{sep}{This group holds data separator information only.
#' This is one of the most important parameters for data reading and output.
#' It is a separate category because it is frequently used and is an input
#' parameter for other subsequent parameters to be read.}
#' @return \item{profile}{This group contains basic information about the
#' dataset, such as the dataset name and a color label to distinguish the
#' dataset. The dataset name is extremely important. This parameter is used
#' almost throughout the data processing of the itol.toolkit package.
#' With the content of this parameter as the key value, the data and theme
#' information of the dataset are associated. In turn, high throughput
#' learning and writing of large-scale data can be achieved. This parameter
#' is not included in some template types with a particularly simple structure,
#' so we choose a file name or a user-defined method as the key value.}
#' @return \item{field}{This group contains information about each sample
#' within the dataset, and this type of parameter exists only for multi-sample
#' data. This information even includes the clustering tree between samples.
#' This information is usually stored as part of the column names in the
#' metadata part or abundance information of the itol.hub object.}
#' @return \item{common_themes}{These themes are used at high frequency in
#' different templates. These parameters are small in number but constitute some
#' common features of iTOL visual style settings, such as legend, margin, etc.}
#' @return \item{specific_themes}{These themes are used only in specific
#' templates. The number of these parameters is very large. However, most of
#' them are used in only one template to control the style details of the
#' visualization. By unifying these parameters and calling them according to the
#' template type, users can perform secondary development and data processing
#' with a high degree of parameter aggregation without worrying too much about
#' the differences between different template types.}
#' @return \item{data}{This slot contains a list of two data frames with the
#' nodes and tips data separately. The first column of the two data frames is
#' the node or tip id. If the input data contains range id, it would be
#' converted to node id by the convert_range_to_node function automatically.}
#'
#' @importFrom dplyr summarise_all n_distinct mutate_all case_when select filter group_by summarize arrange pull mutate
#' @importFrom stats setNames
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_length
#' @importFrom grDevices boxplot.stats
#' @importFrom stringdist stringsim
#' @export
#' @examples
#' tree <- system.file("extdata","tree_of_itol_templates.tree",package = "itol.toolkit")
#' data("template_groups")
#' data("template_parameters_count")
#' # COLLAPSE
#' group_names <- unique(template_groups$group)
#' object <- create_hub(tree = tree)
#' unit <- create_unit(data = group_names, key = "E001_collapse_1",
#' type = "COLLAPSE", tree = tree)
#' object <- learn_data_from_unit(object,unit)
#' # PRUNE
#' select_note = c("theme_style","basic_plot")
#' unit <- create_unit(data = select_note, key = "E002_prune_1",
#' type = "PRUNE", tree = tree)
#' object <- learn_data_from_unit(object,unit)
#' # SPACING
#' df_values = data.frame(id = row.names(template_parameters_count),
#' values = rowSums(template_parameters_count))
#' unit <- create_unit(data = df_values, key = "E005_spacing_1",
#' type = "SPACING", tree = tree)
#' object <- learn_data_from_unit(object,unit)
#' # TREE_COLORS
#' ## range
#' unit <- create_unit(data = template_groups,
#' key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "range",
#' tree = tree)
#' object <- learn_data_from_unit(object,unit)
create_unit <- function(data,key,type,style="default",subtype=NULL,color=NULL,line_type=NULL,font_type=NULL,size_factor=NULL,position=NULL,background_color=NULL,rotation=NULL,method=NULL,shape=NULL,fill=NULL,tree){
  type <- correct_type(type)
  data_left <- learn_df(tree = tree, node = T, tip = T)
  theme <- use.theme(type,style)
  sep <- theme@sep
  profile <- theme@profile
  field <- theme@field
  common_themes <- theme@common_themes
  specific_themes <- theme@specific_themes
  if (type == "COLLAPSE") {
    if(!is.vector(data)){
      stop("The input data class should be a vector")
    }
    df_data = data.frame(id = data, COLLAPSE = TRUE)
    names(df_data)[2] <- stringr::str_replace(names(df_data)[2], "^", paste0(key,"$"))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if (type == "PRUNE") {
    if(!is.vector(data)){
      stop("The input data class should be a vector")
    }
    df_data = data.frame(id = data, PRUNE = TRUE)
    names(df_data)[2] <- stringr::str_replace(names(df_data)[2], "^", paste0(key,"$"))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if (type == "SPACING") {
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the first column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    if(length(names(data)) != 2){
      stop("The input data should has 2 columns")
    }
    names(data) <- c("id",paste0(key,"$VERTICAL_SPACING_FACTOR"))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "TREE_COLORS"){
    if (is.null(subtype)) {
       stop("The subtype parameter is empty")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    colname_subtype = ""
    if(any(data %>% summarise_all(n_distinct) == 1)){
      potential_subtypes = names(data)[data %>% summarise_all(n_distinct) == 1]
      for(potential_subtype in potential_subtypes){
        if(unique(data[[potential_subtype]]) %in% c("range", "clade", "branch", "label", "label_background")){
          if(unique(data[[potential_subtype]]) != subtype || is.null(subtype)){
            message(paste0("Using following column as subtype parameter: ",potential_subtype))
            subtype = unique(data[[potential_subtype]])
            colname_subtype = potential_subtype
          }
        }
      }
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(data[[potential_color]]))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }
    colname_line_type = ""
    if(is.null(line_type)){
      if(subtype %in% c("clade","branch")){
        potential_line_types <- names(data)[-1]
        for(potential_line_type in potential_line_types){
          if(all(unique(data[[potential_line_type]]) %in% c("normal","dashed"))){
            message(paste0("Using following column as line type parameter: ", potential_line_type))
            line_type = data[[potential_line_type]]
            colname_line_type = potential_line_type
          }
        }
      }
    }else {
      if(!subtype %in% c("clade","branch")){
        warning(paste0("Unsupported line type parameter in subtype: ",subtype))
        line_type = NULL
      }
    }
    colname_font_type = ""
    if(is.null(font_type)){
      if(subtype == "label"){
        potential_font_types <- names(data)[-1]
        for(potential_font_type in potential_font_types){
          if(all(unique(data[[potential_font_type]]) %in% c("bold", "italic", "bold-italic", "", "normal"))){
            message(paste0("Using following column as font type parameter: ", potential_font_type))
            font_type = data[[potential_font_type]]
            colname_font_type = potential_font_type
          }
        }
      }
    }else {
      if(subtype != "label"){
        warning(paste0("Unsupported font type parameter in subtype: ",subtype))
        font_type = NULL
      }
    }
    colname_size_factor = ""
    if(is.null(size_factor)){
      if(subtype %in% c("clade","branch","label")){
        potential_size_factors <- names(data)[-1]
        for (potential_size_factor in potential_size_factors) {
          if(stringr::str_remove_all(paste0(data[[potential_size_factor]],collapse = ""),"[\\d\\.]") == ""){
            message(paste0("Using following column as size factor parameter: ", potential_size_factor))
            size_factor = data[[potential_size_factor]]
            colname_size_factor = potential_size_factor
          }
        }
      }
    }else {
      if(!subtype %in% c("clade","branch","label")){
        warning(paste0("Unsupported size factor parameter in subtype: ",subtype))
        size_factor = NULL
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id",colname_subtype,colname_color,colname_line_type,colname_size_factor)]
    # If dual-factor coloring is enabled and data column cannot be uniquely identified, use main grouping column as label column by default
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      if(length(colname_data) != 1){
        colname_data <- color[1]
      }
    }
    # Determine which column name should be used for the last column (LABEL) in DATA block:
    # - Dual-factor mode: use second factor (color[2]) as label
    # - Other cases: use auto-identified colname_data
    label_colname <- colname_data
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      label_colname <- color[2]
    }

    if(subtype == "range"){
      # For dual-factor: sort DATA and color by main factor grouping, then alphabetically by secondary factor within groups
      if(exists("color_level_main") && exists("color_level_gradient") &&
         (color_level_main %in% names(data)) && (color_level_gradient %in% names(data))){
        # Ensure both main and secondary factors are sorted alphabetically
        ord_dual <- order(as.character(data[[color_level_main]]), as.character(data[[color_level_gradient]]))
        data  <- data[ord_dual, , drop = FALSE]
        if(length(color) == nrow(data)){
          color <- color[ord_dual]
        }
      }
      if(length(colname_data)!=1){
        stop("Unable to indentify data column")
      }
    }else {
      if(length(color) != nrow(data)){
        message("Identifying data column to auto setup color parameter")
        if(length(colname_data)!=1){
          stop("Unable to indentify data column")
        }
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          # Support dual-factor coloring: when color is provided as column names in data, allow color to be length==1 but not built-in palette/color values
          if(color %in% names(data)){
            # If only one column name is passed, automatically map using categorical palette
            color_levels = get_color(length(unique(data[[color]])))
            color = as.factor(data[[color]])
            levels(color) <- color_levels
            colname_color = color
          }else{
            stop("Unsupported color parameter")
          }
        }
      }
    }

    # When color is specified as two field names, enable dual-factor (main grouping + gradient) strategy
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      color_level_main     <- color[1]
      color_level_gradient <- color[2]

      # Optional 3rd element as color palette set
      color_set <- NULL
      if(length(color) >= 3){
        candidate_set <- color[3]
        if(stringr::str_remove(candidate_set,"_.*$") %in% get_color(set = "ls")){
          color_set <- candidate_set
        }
      }

      main_vec     <- as.factor(data[[color_level_main]])
      gradient_vec <- data[[color_level_gradient]]

      # Ensure main factor is sorted alphabetically
      main_levels <- sort(levels(main_vec))
      main_vec <- factor(main_vec, levels = main_levels)
      n_main      <- length(main_levels)

      base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)

      color_out <- character(nrow(data))
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        for(j in seq_along(idx_main)){
          row_idx  <- idx_main[j]
          grad_val <- grad_vals[j]
          color_out[row_idx] <- grad_color_map[[ as.character(grad_val) ]]
        }
      }
      color <- factor(color_out)
      # Legend: for dual-factor, labels use secondary factor name; order by main factor grouping, then alphabetically within groups
      common_themes$legend$title <- color_level_gradient
      legend_labels <- character(0)
      legend_colors <- character(0)
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        legend_labels <- c(legend_labels, as.character(unique_grads))
        legend_colors <- c(legend_colors, as.character(grad_color_map[as.character(unique_grads)]))
      }
      common_themes$legend$labels <- legend_labels
      common_themes$legend$colors <- legend_colors
      common_themes$legend$shapes <- rep(1, length(legend_labels))
    }
    if(subtype %in% c("clade","branch")){
      if(is.null(line_type)){
        message("Setting line type parameter as normal")
        line_type = "normal"
      }
      if(length(line_type)==1){
        if(line_type %in% c("normal","dashed")){
          line_type = rep(line_type,nrow(data))
        }else{
          stop("Unsupported line type parameter")
        }
      }
      if(length(line_type)!=nrow(data)){
        stop("Unequal vector length of line type parameter")
      }
    }
    if(subtype == "label"){
      if(is.null(font_type)){
        message("Setting font type parameter as normal")
        font_type = "normal"
      }
      if(length(font_type)==1){
        if(font_type %in% c("bold", "italic", "bold-italic", "", "normal")){
          font_type = rep(font_type,nrow(data))
        }else{
          stop("Unsupported font type parameter")
        }
      }
      if(length(font_type)!=nrow(data)){
        stop("Unequal vector length of font type parameter")
      }
    }
    if(subtype %in% c("clade","branch","label")){
      if(is.null(size_factor)){
        message("Setting size_factor parameter as 1")
        size_factor = 1
      }
      if(length(size_factor)==1){
        if(stringr::str_remove_all(size_factor,"[\\d\\.]") == ""){
          size_factor = rep(size_factor,nrow(data))
        }else{
          stop("Unsupported size factor parameter")
        }
      }
      if(length(size_factor)!=nrow(data)){
        stop("Unequal vector length of size factor parameter")
      }
    }
    if(subtype == "range"){
      if(length(names(data)) > 4){
        stop("The input data should has 2-4 columns: id, type(optional), color(optional), label")
      }
      # If dual-factor exists, include hidden sorting keys to ensure final DATA is ordered by main factor + secondary factor alphabetically
      if(exists("color_level_main") && exists("color_level_gradient") &&
         (color_level_main %in% names(data)) && (color_level_gradient %in% names(data))){
        df_data <- data.frame(id = data[["id"]],
                              subtype = subtype,
                              color = color,
                              label = data[[label_colname]],
                              .__main = as.character(data[[color_level_main]]),
                              .__grad = as.character(data[[color_level_gradient]]))
        names(df_data)[2:4] <- c(paste0(key,c("$TYPE", "$COLOR", "$LABEL_OR_STYLE")))
        
        df_data <- convert_range_to_node(df_data, tree)
        
        if(all(c(".__main",".__grad") %in% names(df_data))){
          ord_final <- order(as.character(df_data$.__main), as.character(df_data$.__grad))
          df_data <- df_data[ord_final, , drop = FALSE]
          
          df_data$.__main <- NULL
          df_data$.__grad <- NULL
          
          # Rebuild legend based on actual order of DATA block
          # Extract unique label and color combinations from sorted data
          sorted_labels <- as.character(df_data[[paste0(key, "$LABEL_OR_STYLE")]])
          sorted_colors <- as.character(df_data[[paste0(key, "$COLOR")]])
          
          # Get unique combinations, preserving order
          unique_combinations <- unique(data.frame(
            label = sorted_labels,
            color = sorted_colors,
            stringsAsFactors = FALSE
          ))
          
          common_themes$legend$labels <- unique_combinations$label
          common_themes$legend$colors <- unique_combinations$color
          common_themes$legend$shapes <- rep(1, length(unique_combinations$label))
        }
      }else{
        df_data <- data.frame(id = data[["id"]],subtype = subtype,color = color,label=data[[label_colname]])
        names(df_data) <- c("id",paste0(key,c("$TYPE", "$COLOR", "$LABEL_OR_STYLE")))
        df_data <- convert_range_to_node(df_data, tree)
      }
      
      # Add global order marker to df_data to ensure correct order is maintained in write_unit
      if(exists("color_level_main") && exists("color_level_gradient") &&
         (color_level_main %in% names(data)) && (color_level_gradient %in% names(data))){
        df_data$.__global_order <- seq_len(nrow(df_data))
      }
      
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
    if(subtype == "clade"){
      if(length(names(data)) > 6){
        stop("The input data should has 6 columns: id, type(optional), color(optional), line type(optional), line width(optional), data(optional. for auto color)")
      }
      df_data <- data.frame(id = data[["id"]], subtype = subtype, color = color, line_type = line_type, line_width = size_factor)
      names(df_data) <- c("id",paste0(key,c("$TYPE", "$COLOR", "$LABEL_OR_STYLE", "$SIZE_FACTOR")))
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
    if(subtype == "branch"){
      if(length(names(data)) > 6){
        stop("The input data should has 6 columns: id, type(optional), color(optional), line type(optional), line width(optional), data(optional. for auto color)")
      }
      df_data <- data.frame(id = data[["id"]], subtype = subtype, color = color, line_type = line_type, line_width = size_factor)
      names(df_data) <- c("id",paste0(key,c("$TYPE", "$COLOR", "$LABEL_OR_STYLE", "$SIZE_FACTOR")))
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
    if(subtype == "label"){
      if(length(names(data)) > 6){
        stop("The input data should has 5 columns: id, type(optional), color(optional), font type(optional), font size(optional), data(optional. for auto color)")
      }
      df_data <- data.frame(id = data[["id"]], subtype = subtype, color = color, font_type = font_type, font_size = size_factor)
      names(df_data) <- c("id",paste0(key,c("$TYPE", "$COLOR", "$LABEL_OR_STYLE", "$SIZE_FACTOR")))
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
    if(subtype == "label_background"){
      if(length(names(data)) > 4){
        stop("The input data should has 1-4 columns: id, type(optional), color(optional), data(optional. for auto color)")
      }
      df_data <- data.frame(id = data[["id"]], subtype = subtype, color = color)
      names(df_data) <- c("id",paste0(key,c("$TYPE", "$COLOR")))
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
  }
  if (type == "DATASET_STYLE") {
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the first column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    colname_subtype = ""
    if(is.null(subtype)){
      potential_subtypes <- names(data)[-1]
      for (potential_subtype in potential_subtypes) {
         if (all(unique(data[[potential_subtype]]) %in% c("branch","label"))) {
            message(paste0("Using following column as subtype parameter: ", potential_subtype))
            subtype = data[[potential_subtype]]
            colname_subtype = potential_subtype
         }
      }
    }
    colname_position = ""
    if(is.null(position)){
      potential_positions <- names(data)[-1]
      for (potential_position in potential_positions) {
         if (all(unique(data[[potential_position]]) %in% c("node","clade"))) {
            message(paste0("Using following column as position parameter: ", potential_position))
            position = data[[potential_position]]
            colname_position = potential_position
         }
      }
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(data[[potential_color]]))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }
    colname_size_factor = ""
    if(is.null(size_factor)){
      potential_size_factors <- names(data)[-1]
      for (potential_size_factor in potential_size_factors) {
        if(stringr::str_remove_all(paste0(data[[potential_size_factor]],collapse = ""),"[\\d\\.]") == ""){
          message(paste0("Using following column as size factor parameter: ", potential_size_factor))
          size_factor = data[[potential_size_factor]]
          colname_size_factor = potential_size_factor
        }
      }
    }
    colname_line_type = ""
    if(is.null(line_type)){
      if(subtype == "branch"){
        potential_line_types <- names(data)[-1]
        for(potential_line_type in potential_line_types){
          if(all(unique(data[[potential_line_type]]) %in% c("normal","dashed"))){
            message(paste0("Using following column as line type parameter: ", potential_line_type))
            line_type = data[[potential_line_type]]
            colname_line_type = potential_line_type
          }
        }
      }
    }else {
      if(subtype != "branch"){
        warning(paste0("Unsupported line type parameter in subtype: ",subtype))
        line_type = NULL
      }
    }
    colname_font_type = ""
    if(is.null(font_type)){
      if(subtype == "label"){
        potential_font_types <- names(data)[-1]
        for(potential_font_type in potential_font_types){
          if(all(unique(data[[potential_font_type]]) %in% c("bold", "italic", "bold-italic", "", "normal"))){
            message(paste0("Using following column as font type parameter: ", potential_font_type))
            font_type = data[[potential_font_type]]
            colname_font_type = potential_font_type
          }
        }
      }
    }else {
      if(subtype != "label"){
        warning(paste0("Unsupported font type parameter in subtype: ",subtype))
        font_type = NULL
      }
    }
    colname_background_color = ""
    if(is.null(background_color)){
      if(subtype == "label"){
        potential_background_colors <- names(data)[-1]
        for(potential_background_color in potential_background_colors){
          if(min(stringr::str_length(data[[potential_background_color]]))>3){
            str_1 <- unique(stringr::str_extract(data[[potential_background_color]],"^."))
            str_2 <- unique(stringr::str_extract(data[[potential_background_color]],"^..."))
            if(length(str_1)==1){
              if(str_1 == "#"){
                message(paste0("Using following column as background color parameter: ", potential_background_color))
                background_color = data[[potential_background_color]]
                colname_background_color = potential_background_color
              }
            }
            if(length(str_2)==1){
              if(str_2 == "rgb"){
                message(paste0("Using following column as background color parameter: ", potential_background_color))
                background_color = data[[potential_background_color]]
                colname_background_color = potential_background_color
              }
            }
          }
        }
      }
    }else {
      if(subtype != "label"){
        warning(paste0("Unsupported background color parameter in subtype: ",subtype))
        background_color = NULL
      }
    }
    if (is.null(subtype)) {
      message("The subtype parameter is empty. Using label as subtype.")
      subtype = "label"
    }
    if (is.null(position)) {
      message("The position parameter is empty. Using node as position.")
      position = "node"
    }
    if (is.null(size_factor)) {
      message("The size factor parameter is empty. Using 1 as size factor.")
      size_factor = 1
    }
    if (is.null(line_type)) {
      message("The line type parameter is empty. Using normal as line type.")
      line_type = "normal"
    }
    if (is.null(font_type)) {
      message("The font type parameter is empty. Using normal as font type.")
      font_type = "normal"
    }
    if(length(background_color)==1){
      background_color = rep(background_color,nrow(data))
    }
    colname_data <- names(data)[!names(data)%in%c("id",colname_subtype, colname_position, colname_color, colname_size_factor, colname_line_type, colname_font_type, colname_background_color)]
    # If dual-factor coloring is enabled and data column cannot be uniquely identified, use main grouping column as label column by default
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      if(length(colname_data) != 1){
        colname_data <- color[1]
      }
    } else {
      if(length(color) != nrow(data)){
        message("Identifying data column to auto setup color parameter")
        if(length(colname_data)!=1){
          # If background_color is dual-factor, use the gradient column as data column
          if(!is.null(background_color) && length(background_color) >= 2 && 
             all(background_color[1:2] %in% names(data))){
            colname_data <- background_color[2]
          } else {
            stop("Unable to indentify data column")
          }
        }
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    
    # ------------------ Start dual-factor coloring code for DATASET_STYLE ------------------
    
    # When color is specified as two field names, enable dual-factor (main grouping + gradient) strategy
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      color_level_main     <- color[1]
      color_level_gradient <- color[2]
      color_param_original <- color  # Save original color parameter for background_color comparison
      
      # Optional 3rd element as color palette set
      color_set <- NULL
      if(length(color) >= 3){
        candidate_set <- color[3]
        if(stringr::str_remove(candidate_set,"_.*$") %in% get_color(set = "ls")){
          color_set <- candidate_set
        }
      }
      
      main_vec     <- as.factor(data[[color_level_main]])
      gradient_vec <- data[[color_level_gradient]]
      
      # Ensure main factor is sorted alphabetically
      main_levels <- sort(levels(main_vec))
      main_vec <- factor(main_vec, levels = main_levels)
      n_main      <- length(main_levels)
      
      base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)
      
      color_out <- character(nrow(data))
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        for(j in seq_along(idx_main)){
          row_idx  <- idx_main[j]
          grad_val <- grad_vals[j]
          color_out[row_idx] <- grad_color_map[[ as.character(grad_val) ]]
        }
      }
      color <- factor(color_out)
      
      # Handle background_color dual-factor coloring if it matches color parameter
      if(!is.null(background_color) && length(background_color) >= 2 && 
         all(background_color[1:2] == color_param_original[1:2])){
        # Dual-factor background coloring: same logic as color but adjusted 70% towards white
        background_color_out <- character(nrow(data))
        for(i in seq_len(n_main)){
          this_main  <- main_levels[i]
          base_color <- base_colors_main[i]
          idx_main   <- which(main_vec == this_main)
          grad_vals    <- gradient_vec[idx_main]
          unique_grads <- sort(unique(grad_vals))
          n_grads      <- length(unique_grads)
          colors_grad  <- gradient_color(n_grads, base_color)
          grad_color_map <- setNames(colors_grad, unique_grads)
          for(j in seq_along(idx_main)){
            row_idx  <- idx_main[j]
            grad_val <- grad_vals[j]
            original_bg_color <- grad_color_map[[ as.character(grad_val) ]]
             # Smart background color selection based on color distance to white
             # First try 70% towards white
             bg_gradient_white <- gradient_color(10, original_bg_color, end = "#FFFFFF")
             bg_color_white <- bg_gradient_white[7]  # 70% towards white
             
             # Calculate distances
             dist_bg_white <- color_distance(bg_color_white, "#FFFFFF")
             
             # If background color is too close to white (distance < 80), use smart darkening instead
             if(dist_bg_white < 80){
               # Use smart darkening to preserve color hue and saturation
               background_color_out[row_idx] <- darken_color(original_bg_color, factor = 0.4, method = "enhanced")
             } else {
               background_color_out[row_idx] <- bg_color_white
             }
          }
        }
        background_color <- background_color_out
      }
      
      # Legend: for dual-factor, labels use secondary factor name; order by main factor grouping, then alphabetically within groups
      common_themes$legend$title <- color_level_gradient
      legend_labels <- character(0)
      legend_colors <- character(0)
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        legend_labels <- c(legend_labels, as.character(unique_grads))
        legend_colors <- c(legend_colors, as.character(grad_color_map[as.character(unique_grads)]))
      }
      common_themes$legend$labels <- legend_labels
      common_themes$legend$colors <- legend_colors
      common_themes$legend$shapes <- rep(1, length(legend_labels))
    }
    
    # ------------------- End dual-factor coloring code for DATASET_STYLE -------------------
    
    # Save original color parameter for comparison
    color_param_original <- color
    
    # Single factor coloring logic (existing code)
    if(length(color) == 1 && !exists("color_level_main")){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
      
      # Handle background_color dual-factor when color is single factor
      if(!is.null(background_color) && length(background_color) >= 2 && 
         all(background_color[1:2] %in% names(data))){
        
        background_color_level_main     <- background_color[1]
        background_color_level_gradient <- background_color[2]
        
        # Optional 3rd element as color palette set
        background_color_set <- NULL
        if(length(background_color) >= 3){
          candidate_set <- background_color[3]
          if(stringr::str_remove(candidate_set,"_.*$") %in% get_color(set = "ls")){
            background_color_set <- candidate_set
          }
        }
      
        main_vec     <- as.factor(data[[background_color_level_main]])
        gradient_vec <- data[[background_color_level_gradient]]
      
        # Ensure main factor is sorted alphabetically
        main_levels <- sort(levels(main_vec))
        main_vec <- factor(main_vec, levels = main_levels)
        n_main      <- length(main_levels)
      
        base_colors_main <- if(is.null(background_color_set)) get_color(n_main) else get_color(n_main, set = background_color_set)
      
        background_color_out <- character(nrow(data))
        for(i in seq_len(n_main)){
          this_main  <- main_levels[i]
          base_color <- base_colors_main[i]
          idx_main   <- which(main_vec == this_main)
          grad_vals    <- gradient_vec[idx_main]
          unique_grads <- sort(unique(grad_vals))
          n_grads      <- length(unique_grads)
          colors_grad  <- gradient_color(n_grads, base_color)
          grad_color_map <- setNames(colors_grad, unique_grads)
          for(j in seq_along(idx_main)){
            row_idx  <- idx_main[j]
            grad_val <- grad_vals[j]
            original_bg_color <- grad_color_map[[ as.character(grad_val) ]]
            # Only apply smart background color adjustment when color and background_color parameters are identical
            # When they are different, use original gradient colors directly
            # Check if the original color parameter was a single black color
            if(exists("color_param_original") && length(color_param_original) == 1 && color_param_original == "#000000"){
              # When color is single black and background_color is dual-factor, use original gradient colors
              background_color_out[row_idx] <- original_bg_color
            } else {
              # Smart background color selection based on color distance to white
              # First try 70% towards white
              bg_gradient_white <- gradient_color(10, original_bg_color, end = "#FFFFFF")
              bg_color_white <- bg_gradient_white[7]  # 70% towards white
              
              # Calculate distances
              dist_bg_white <- color_distance(bg_color_white, "#FFFFFF")
              
              # If background color is too close to white (distance < 80), use smart darkening instead
              if(dist_bg_white < 80){
                # Use smart darkening to preserve color hue and saturation
                background_color_out[row_idx] <- darken_color(original_bg_color, factor = 0.4, method = "enhanced")
              } else {
                background_color_out[row_idx] <- bg_color_white
              }
            }
          }
        }
        background_color <- background_color_out
        
        # Update legend for background color dual-factor
        common_themes$legend$title <- background_color_level_gradient
        legend_labels <- character(0)
        legend_colors <- character(0)
        for(i in seq_len(n_main)){
          this_main  <- main_levels[i]
          base_color <- base_colors_main[i]
          idx_main   <- which(main_vec == this_main)
          grad_vals    <- gradient_vec[idx_main]
          unique_grads <- sort(unique(grad_vals))
          n_grads      <- length(unique_grads)
          colors_grad  <- gradient_color(n_grads, base_color)
          grad_color_map <- setNames(colors_grad, unique_grads)
          legend_labels <- c(legend_labels, as.character(unique_grads))
          # Use the same logic for legend colors as background colors
          for(grad_val in unique_grads){
            original_bg_color <- grad_color_map[[ as.character(grad_val) ]]
            # Only apply smart background color adjustment when color and background_color parameters are identical
            if(exists("color_param_original") && length(color_param_original) == 1 && color_param_original == "#000000"){
              # When color is single black and background_color is dual-factor, use original gradient colors
              legend_colors <- c(legend_colors, original_bg_color)
            } else {
              # Smart background color selection based on color distance to white
              bg_gradient_white <- gradient_color(10, original_bg_color, end = "#FFFFFF")
              bg_color_white <- bg_gradient_white[7]  # 70% towards white
              dist_bg_white <- color_distance(bg_color_white, "#FFFFFF")
              if(dist_bg_white < 80){
                legend_colors <- c(legend_colors, darken_color(original_bg_color, factor = 0.4, method = "enhanced"))
              } else {
                legend_colors <- c(legend_colors, bg_color_white)
              }
            }
          }
        }
        common_themes$legend$labels <- legend_labels
        common_themes$legend$colors <- legend_colors
        common_themes$legend$shapes <- rep(1, length(legend_labels))
      }
    }
    if(subtype == "branch"){
      if(length(names(data)) > 6){
        stop("The input data should has 6 columns: id, type(optional), position(optional), color(optional), line width(optional), line type(optional), data(optional. for auto color)")
      }
      df_data <- data.frame(id = data[["id"]], subtype = subtype, position = position, color = color, line_width = size_factor, line_type = line_type)
      names(df_data) <- c("id",paste0(key,c("$TYPE", "$WHAT", "$COLOR", "$WIDTH_OR_SIZE_FACTOR", "$STYLE")))
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      profile$name <- key
      
      # Build legend for branch subtype
      if(!exists("color_level_main")){
        # Single factor mode - use colname_data for legend
        if(!is.null(colname_data) && colname_data != ""){
          common_themes$legend$title <- colname_data
          unique_data <- data.frame(label = data[[colname_data]], color = color, stringsAsFactors = FALSE)
          unique_data <- unique_data[!duplicated(unique_data$label), ]
          common_themes$legend$colors <- unique_data$color
          common_themes$legend$labels <- unique_data$label
          common_themes$legend$shapes <- rep(1, length(common_themes$legend$labels))
        }
      }
      
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
    if(subtype == "label"){
      if(length(names(data)) > 6){
        stop("The input data should has 6 columns: id, type(optional), position(optional), color(optional), font size(optional), font type(optional), background color(optional), data(optional. for auto color)")
      }
      
      # Determine label column name
      label_colname <- colname_data
      if(length(color) >= 2 && all(color[1:2] %in% names(data))){
        label_colname <- color[2]
      } else if(length(color) == 1 && !is.null(background_color) && length(background_color) >= 2 && 
                all(background_color[1:2] %in% names(data))){
        label_colname <- background_color[2]
      }
      
      if(is.null(background_color)){
        df_data <- data.frame(id = data[["id"]], subtype = subtype, position = position, color = color, font_size = size_factor, font_type = font_type, label = data[[label_colname]])
        names(df_data) <- c("id",paste0(key,c("$TYPE", "$WHAT", "$COLOR", "$WIDTH_OR_SIZE_FACTOR", "$STYLE", "$LABEL_OR_STYLE")))
      }else {
        df_data <- data.frame(id = data[["id"]], subtype = subtype, position = position, color = color, font_size = size_factor, font_type = font_type, background_color = background_color, label = data[[label_colname]])
        names(df_data) <- c("id",paste0(key,c("$TYPE", "$WHAT", "$COLOR", "$WIDTH_OR_SIZE_FACTOR", "$STYLE", "$BACKGROUND_COLOR", "$LABEL_OR_STYLE")))
      }
      df_data <- convert_range_to_node(df_data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
      profile$name <- key
      
      # Build legend for label subtype
      if(!exists("color_level_main")){
        # Single factor mode - use colname_data for legend
        if(!is.null(colname_data) && colname_data != ""){
          # If background_color is dual-factor, use background colors for legend
          # Check original background_color parameter before it was processed
          if(exists("background_color_level_main")){
            # Legend already built in background_color dual-factor section above
            # No need to rebuild here
          } else {
            # Regular single factor legend
            common_themes$legend$title <- colname_data
            unique_data <- data.frame(label = data[[colname_data]], color = color, stringsAsFactors = FALSE)
            unique_data <- unique_data[!duplicated(unique_data$label), ]
            common_themes$legend$colors <- unique_data$color
            common_themes$legend$labels <- unique_data$label
            common_themes$legend$shapes <- rep(1, length(common_themes$legend$labels))
          }
        }
      }
      
      unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
    }
  }
  if (type == "DATASET_RANGE"){

  }
  if(type == "LABELS"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the first column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    if(length(names(data)) != 2){
      stop("The input data should has 2 columns")
    }
    names(data) <- c("id",paste0(key,"$LABEL"))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_TEXT"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the first column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    if(!"new_label" %in% names(data)){
      message(paste0("Using the second column as new label: ",names(data)[2]))
      names(data)[2] <- "new_label"
    }
    if(length(names(data)) > 8){
      stop("The input data should has 2-8 columns: id, label, position(optional), color(optional), style(optional), size factor(optional), rotation(optional), data(optional. for auto color)")
    }
    colname_position = ""
    if(is.null(position)){
      potential_positions <- names(data)[-1]
      for (potential_position in potential_positions) {
        if(stringr::str_remove_all(paste0(data[[potential_position]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_position]],collapse = ""),"\\.") <= 1){
          potential_values <- as.numeric(data[[potential_position]])
          if(all((potential_values >= -1) & (potential_values <= 1))){
            message(paste0("Using following column as position parameter: ", potential_position))
            position = data[[potential_position]]
            colname_position = potential_position
            break
          }
        }
      }
    }
    colname_font_size = ""
    if(is.null(size_factor)){
      potential_font_sizes <- names(data)[!names(data)%in%c("id", colname_position)]
      for (potential_font_size in potential_font_sizes) {
        if(stringr::str_remove_all(paste0(data[[potential_font_size]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_font_size]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as font size parameter: ", potential_font_size))
          size_factor = data[[potential_font_size]]
          colname_font_size = potential_font_size
          break
        }
      }
    }
    colname_rotation = ""
    if(is.null(rotation)){
      potential_rotations <- names(data)[!names(data)%in%c("id", colname_position, colname_font_size)]
      for (potential_rotation in potential_rotations) {
        if(stringr::str_remove_all(paste0(data[[potential_rotation]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_rotation]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as rotation parameter: ", potential_rotation))
          rotation = data[[potential_rotation]]
          colname_rotation = potential_rotation
          break
        }
      }
    }
    colname_font_type = ""
    if(is.null(font_type)){
      potential_font_types <- names(data)[-1]
      for(potential_font_type in potential_font_types){
        if(all(unique(data[[potential_font_type]]) %in% c("bold", "italic", "bold-italic", "", "normal"))){
          message(paste0("Using following column as font type parameter: ", potential_font_type))
          font_type = data[[potential_font_type]]
          colname_font_type = potential_font_type
        }
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id","new_label", colname_position, colname_rotation, colname_font_type, colname_font_size)]
    if(length(color) == 1){
      if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
        color = rep(color,nrow(data))
      }
    }
    if(length(color) != nrow(data)){
      message("Identifying data column to auto setup color parameter")
      if(length(colname_data)!=1){
        stop("Unable to indentify data column")
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
    }
    if(is.null(position)){
      message("The position parameter is empty. Using 0 as position.")
      position = 0
    }
    if (is.null(font_type)) {
      message("The font type parameter is empty. Using normal as font type.")
      font_type = "normal"
    }
    if (is.null(size_factor)) {
      message("The font size parameter is empty. Using 1 as font size.")
      size_factor = 1
    }
    if(is.null(rotation)){
      message("The rotation parameter is empty. Using 0 as rotation.")
      rotation = 0
    }
    df_data <- data.frame(id = data[["id"]], label = data[["new_label"]], position = position, color = color, font_type = font_type, font_size = size_factor, rotation = rotation)
    names(df_data) <- c("id",paste0(key,c("$LABEL", "$POSITION", "$COLOR", "$STYLE", "$SIZE_FACTOR", "$ROTATION")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_COLORSTRIP"){
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(data[[potential_color]]))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id",colname_color)]
    # Check if dual-factor coloring: only check first 2 elements
    color_cols_to_check <- if(length(color) >= 2) color[1:2] else color
    # If dual-factor coloring is enabled and data column cannot be uniquely identified, use main grouping column as label column by default
    if(length(color) >= 2 && all(color_cols_to_check %in% names(data))){
      if(length(colname_data) != 1){
        colname_data <- color[1]
      }
    } else {
      if(length(colname_data)!=1){
        stop("Unable to indentify data column")
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    
    # ------------------ Start dual-factor coloring code for DATASET_COLORSTRIP ------------------
    
    # When color is specified as two field names, enable dual-factor (main grouping + gradient) strategy
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      color_level_main     <- color[1]
      color_level_gradient <- color[2]
      
      # Optional 3rd element as color palette set
      color_set <- NULL
      if(length(color) >= 3){
        candidate_set <- color[3]
        if(stringr::str_remove(candidate_set,"_.*$") %in% get_color(set = "ls")){
          color_set <- candidate_set
        }
      }
      
      main_vec     <- as.factor(data[[color_level_main]])
      gradient_vec <- data[[color_level_gradient]]
      
      # Ensure main factor is sorted alphabetically
      main_levels <- sort(levels(main_vec))
      main_vec <- factor(main_vec, levels = main_levels)
      n_main      <- length(main_levels)
      
      base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)
      
      color_out <- character(nrow(data))
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        for(j in seq_along(idx_main)){
          row_idx  <- idx_main[j]
          grad_val <- grad_vals[j]
          color_out[row_idx] <- grad_color_map[[ as.character(grad_val) ]]
        }
      }
      color <- factor(color_out)
      
      # Legend: for dual-factor, labels use secondary factor name; order by main factor grouping, then alphabetically within groups
      common_themes$legend$title <- color_level_gradient
      legend_labels <- character(0)
      legend_colors <- character(0)
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        legend_labels <- c(legend_labels, as.character(unique_grads))
        legend_colors <- c(legend_colors, as.character(grad_color_map[as.character(unique_grads)]))
      }
      common_themes$legend$labels <- legend_labels
      common_themes$legend$colors <- legend_colors
      common_themes$legend$shapes <- rep(1, length(legend_labels))
    }
    
    # ------------------- End dual-factor coloring code for DATASET_COLORSTRIP -------------------
    
    # Single factor coloring logic (existing code)
    if(length(color) == 1 && !exists("color_level_main")){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
    }
    if(length(names(data)) > 4){
      stop("The input data should has 2-4 columns: id, type(optional), color(optional), label")
    }
    # Determine which column name should be used for the last column (LABEL) in DATA block:
    # - Dual-factor mode: use second factor (color[2]) as label
    # - Other cases: use auto-identified colname_data
    label_colname <- colname_data
    if(length(color) >= 2 && all(color[1:2] %in% names(data))){
      label_colname <- color[2]
    }
    
    df_data <- data.frame(id = data[["id"]],color = color,label=data[[label_colname]])
    names(df_data) <- c("id",paste0(key,c("$COLOR", "$LABEL")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    
    # Build legend based on coloring mode
    if(!exists("color_level_main")){
      # Single factor mode
      common_themes$legend$title <- colname_data
      
      # Fix legend order issue: follow original data frame order, not factor alphabetical order
      # Get unique labels and corresponding colors, preserving original order
      unique_data <- data.frame(label = data[[colname_data]], color = color, stringsAsFactors = FALSE)
      unique_data <- unique_data[!duplicated(unique_data$label), ]
      
      common_themes$legend$colors <- unique_data$color
      common_themes$legend$labels <- unique_data$label
      common_themes$legend$shapes <- rep(1,length(common_themes$legend$labels))
    }
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if (type == "DATASET_BINARY") {
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    
    # Store original data before any conversion for metadata extraction
    data_original <- data.table::copy(data)
    
    # Apply binary conversion if method is "binary"
    if(!is.null(method) && method == "binary"){
      data <- convert_to_binary(data, force_convert = FALSE, verbose = TRUE)
    }
    
    # NOW check column classes after conversion
    col_classes <- sapply(data, class)
    char_count <- sum(col_classes == "character")
    
    # For metadata, use original data (before conversion)
    col_classes_orig <- sapply(data_original, class)
    char_count_orig <- sum(col_classes_orig == "character")
    
    # Check if this is dual-factor coloring scenario
    # Only transpose data if we have character columns AND they will be used for dual-factor coloring
    is_dual_factor <- FALSE
    if(!is.null(color) && length(color) >= 2){
      # Check if first 2 elements of color could be column names (for dual-factor)
      is_dual_factor <- TRUE
    } else if(!is.null(color) && length(color) == 1 && !is.null(shape)){
      # Check if using shape parameter for dual-factor
      is_dual_factor <- TRUE
    }
    
    if(char_count_orig > 1){
      cols_to_keep <- c(names(data)[1], names(data)[!(col_classes == "character")])
      metacols <- names(data_original)[(col_classes_orig == "character")]
      # Ensure data_original is a data.table for ..metacols syntax
      if(!data.table::is.data.table(data_original)){
        data_original <- data.table::as.data.table(data_original)
      }
      if(!data.table::is.data.table(data)){
        data <- data.table::as.data.table(data)
      }
      data_meta <- data_original[, ..metacols]
      data_sub <- data[, ..cols_to_keep]
      
      # Only transpose if this is a dual-factor coloring scenario
      if(is_dual_factor && length(cols_to_keep) > 1){
        data <- dt_transpose_header(data_sub)
      } else {
        # For single-factor coloring, use numeric columns only (no transpose)
        data <- data_sub
      }
    } else {
      # If no character columns (except ID), create data_meta from original data
      data_meta <- data_original
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 2){
      stop("The input data should has at least 2 columns")
    }
    
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1 && stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
      # Single-factor coloring with color palette
      field_names <- names(data)[-1]
      names(data) <- c("id",paste0(key,"$",field_names))
      data <- convert_range_to_node(data, tree)
      data_left[["node"]] <- df_merge(data_left[["node"]], data)
      data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
      field_length <- length(field_names)
      field$labels <- field_names
      field$colors <- get_color(field_length,set = color)
    }else{
      # Check if dual-factor coloring: first 2 elements should be column names
      # Third element (optional) is color palette
      color_cols_to_check <- if(length(color) >= 2) color[1:2] else color
      
      # Check for col_groups attribute first (DATASET_BINARY with group annotations)
      # Determine which attribute to use based on first element of color
      # If color[1] is "group", use default "col_groups" attribute
      # Otherwise, if color[1] is a valid attribute name, use it
      if(length(color) >= 2 && color[1] != "group" && color[1] %in% names(attributes(data_original))){
        color_attr_name <- color[1]
        col_groups <- attr(data_original, color_attr_name)
        has_col_groups <- !is.null(col_groups) && length(col_groups) > 0
      } else if(length(color) >= 2 && color[1] == "group"){
        color_attr_name <- "col_groups"
        col_groups <- attr(data_original, color_attr_name)
        has_col_groups <- !is.null(col_groups) && length(col_groups) > 0
      } else {
        color_attr_name <- "col_groups"
        col_groups <- NULL
        has_col_groups <- FALSE
      }
      
      # Check if using data_meta columns
      has_data_meta_cols <- all(color_cols_to_check %in% names(data_meta))
      
      if(has_col_groups || has_data_meta_cols){
        color_set <- NULL  # Initialize color_set
        
        # Handle dual-factor coloring based on color parameter length
        if(length(color)==2){
          color_level_main     <- color[1]
          color_level_gradient <- color[2]
          color_set            <- NULL
        }
        if(length(color)==3){
          color_level_main     <- color[1]
          color_level_gradient <- color[2]
          color_set            <- color[3]
        }
        
        # Handle single-factor coloring with shape parameter
        if(length(color)==1 && !is.null(shape) && shape %in% names(data_meta)){
          color_level_main     <- shape
          color_level_gradient <- color
        }
        
        # Check if dual-factor variables are properly set
        if(!exists("color_level_main") || !exists("color_level_gradient")){
          stop("Unable to set up dual-factor coloring variables")
        }
        
        # For dual-factor coloring, we need to preserve the original numeric column names
        # field_names should be the numeric columns from the original data (before transpose)
        # We need to get the numeric columns from the original data structure
        if(char_count > 1){
          # Original data had character and numeric columns
          field_names <- names(data_sub)[!(names(data_sub) %in% names(data_sub)[1])]
          # For dual-factor coloring, use original data structure (no transpose)
          data <- data_sub
        } else {
          # All columns were numeric except ID
          field_names <- names(data)[!(names(data) %in% names(data)[1])]
        }
        # For DATASET_BINARY dual-factor coloring, use original data structure
        # Rename columns to include key prefix
        names(data) <- c("id",paste0(key,"$",names(data)[-1]))
        # Convert range to node and merge with tree data
        data <- convert_range_to_node(data, tree)
        data_left[["node"]] <- df_merge(data_left[["node"]], data)
        data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
        field_length <- length(field_names)
        field$labels <- field_names

        # ------------------ Start dual-factor coloring code ------------------

        # Check if this is a DATASET_BINARY scenario with col_groups attribute
        # col_groups was determined above
        # Note: col_groups might be NULL if no attribute matching color[1] was found
        
        # Check if using separator-based dual-factor coloring
        # If color_level_gradient is not a column name, treat it as separator
        is_separator_mode <- !is.null(col_groups) && length(col_groups) > 0 && 
          (color_level_main == "group" || color_level_main == color_attr_name) &&
          !(color_level_gradient %in% names(data_meta))
        
        if(is_separator_mode){
          
          # Special case: Using col_groups attribute with separator-based dual-factor coloring
          # main = "group": use group names as main factor (e.g., Asia, North_America, South_America)
          # gradient = separator_string: use separator to split column names and group by prefix
          
          # Get the separator string from color_level_gradient
          separator <- color_level_gradient
          
          # Get the main groups (group names)
          main_groups <- names(col_groups)
          n_main <- length(main_groups)
          
          # Generate base colors for main groups
          base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)
          
          # Generate colors for each field based on its group membership
          field$colors <- character(field_length)
          
          for(i in seq_along(field$labels)){
            field_name <- field$labels[i]
            
            # Find which group this field belongs to
            for(j in seq_along(col_groups)){
              if(field_name %in% col_groups[[j]]){
                # This field belongs to group j
                base_color <- base_colors_main[j]
                
                # Get columns in this group
                group_cols <- col_groups[[j]]
                field_index_in_group <- which(group_cols == field_name)
                n_fields_in_group <- length(group_cols)
                
                # Using separator: extract prefix before separator from column names
                if(n_fields_in_group == 1){
                  field$colors[i] <- base_color
                } else {
                  # Extract prefix before separator using regex
                  # Escape special regex characters in separator
                  # Fixed approach: manually escape special chars
                  if(separator %in% c(".", "|", "(", ")", "+", "$", "*", "?", "[", "]", "{", "}", "^", "\\\\")){
                    sep_escaped <- paste0("\\\\", separator)
                  } else {
                    sep_escaped <- separator
                  }
                  prefix_pattern <- paste0(sep_escaped, ".*$")
                  prefixes <- sub(prefix_pattern, "", group_cols)
                  unique_prefixes <- unique(prefixes)
                  n_unique_prefixes <- length(unique_prefixes)
                  
                  if(n_unique_prefixes == 1){
                    # All columns have same prefix, use base color
                    field$colors[i] <- base_color
                  } else {
                    # Generate gradient colors for unique prefixes
                    gradient_colors <- gradient_color(n_unique_prefixes, base_color)
                    field_prefix <- prefixes[field_index_in_group]
                    prefix_index <- which(unique_prefixes == field_prefix)
                    
                    if(length(prefix_index) > 0 && prefix_index >= 1 && prefix_index <= length(gradient_colors)){
                      field$colors[i] <- gradient_colors[prefix_index]
                    } else {
                      field$colors[i] <- base_color
                    }
                  }
                }
                break
              }
            }
          }
          
        } else {
          # Standard dual-factor coloring (using data_meta or other factors)
          # 1. Extract main grouping column and gradient grouping column
          main_vec     <- as.factor(data_meta[[color_level_main]])
          gradient_vec <- data_meta[[color_level_gradient]]

          # 2. Determine how many main groups (levels)
          main_levels <- levels(main_vec)
          n_main      <- length(main_levels)

          # 3. First use get_color() to generate a "base color" for each main group
          #    get_color(n_main) returns a color vector of length n_main
          #    Optional 3rd element as color palette set
          base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)  # e.g., ["#E41A1C", "#377EB8", "#4DAF4A", ...]

          # 4. For DATASET_BINARY dual-factor coloring, generate different colors for each field
          #    Each field should have a distinct color based on the dual-factor scheme
          field$colors <- if(is.null(color_set)) get_color(field_length) else get_color(field_length, set = color_set)
        }

        # ------------------- End dual-factor coloring code -------------------
      } else {
        # Single-factor coloring fallback
        # Define field_names for single-factor mode
        if(char_count_orig > 1){
          # Use numeric columns from the converted data
          field_names <- names(data)[!(names(data) %in% names(data)[1])]
        } else {
          field_names <- names(data)[!(names(data) %in% names(data)[1])]
        }
        # Rename columns and merge data
        names(data) <- c("id",paste0(key,"$",field_names))
        data <- convert_range_to_node(data, tree)
        data_left[["node"]] <- df_merge(data_left[["node"]], data)
        data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
        field_length <- length(field_names)
        field$labels <- field_names
        # Handle color assignment
        if(length(color) == 1){
          # Check if it's a valid hex color or color name
          is_color <- grepl("^#[0-9A-Fa-f]{6}$", color) || color %in% grDevices::colors()
          if(is_color){
            # User specified a specific color, use it for all fields
            field$colors <- rep(color, field_length)
          } else {
            # Not a valid color, use default palette
            field$colors <- get_color(field_length)
          }
        } else {
          # Multiple colors provided, use them
          field$colors <- rep(color, length.out = field_length)
        }
      }

      # Generate legend for DATASET_BINARY
      # Legend should be based on field names (columns)
      common_themes$legend$title <- "Binary Data"
      common_themes$legend$labels <- field$labels
      common_themes$legend$colors <- as.character(field$colors)
      common_themes$legend$shapes <- rep(1, length(field$labels))  # Default shape for binary data

    }
    if(is.null(shape)){
      field$shapes <- rep(2,field_length)
    }else{
      shapes <- c("RE","HH","HV","EL","DI","TR","TL","PL","PR","PU","PD","OC","GP")
      if(shape %in% shapes){
        field$shapes <- rep(shape,field_length)
      }else if(shape == "group" || (shape %in% names(attributes(data_original)))){
        # Check if shape is "group" (default) or a custom attribute name (e.g., "col_groups_color")
        shape_attr_name <- if(shape == "group") "col_groups" else shape
        col_groups <- attr(data_original, shape_attr_name)
        
        if(!is.null(col_groups) && length(col_groups) > 0){
          # Assign shapes based on group membership
          # Map each field to its group and assign shapes accordingly
          field$shapes <- rep(2, field_length)  # Default shape
          
          n_groups <- length(col_groups)
          available_shapes <- 1:min(n_groups, 6)  # iTOL supports shapes 1-6
          
          for(i in seq_along(col_groups)){
            group_name <- names(col_groups)[i]
            group_cols <- col_groups[[i]]
            shape_idx <- available_shapes[(i - 1) %% length(available_shapes) + 1]
            
            # Find field indices that belong to this group
            for(j in seq_along(field$labels)){
              if(field$labels[j] %in% group_cols){
                field$shapes[j] <- shape_idx
              }
            }
          }
        } else {
          # No col_groups attribute, use default
          field$shapes <- rep(2, field_length)
        }
      }else{
        if(shape %in% names(data_meta)){
          # For DATASET_BINARY, field$shapes should have length = field_length (number of fields)
          # Each field gets a different shape based on the unique values in the shape column
          unique_shapes <- unique(data_meta[[shape]])
          n_shapes <- length(unique_shapes)
          
          # Assign shapes to fields (cycle through available shapes if needed)
          field$shapes <- rep(1:min(n_shapes, field_length), length.out = field_length)
        } else if(is.numeric(shape)){
          # If shape is a numeric value (1-6 for iTOL shape types)
          field$shapes <- rep(shape, field_length)
        } else {
          # Default fallback
          field$shapes <- rep(2, field_length)
        }
      }
    }

    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_GRADIENT"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(is.null(method)){
      method = "sum"
    }else {
       if(!method %in% c("sum","mean")){
        warning("Unsupported method. Using sum as method parameter")
        method = "sum"
       }
    }
    if(length(names(data)) > 2){
      message("The input data are mutate as one column by method: ", method)
    }
    field_name = names(data)[2]
    if(length(names(data))>2){
      field_name <- paste0(method," of ", paste(names(data)[-1],collapse = ", "))
    }
    id <- data[["id"]]
    data <- data[,-1]
    data[is.na(data)] <- 0
    method = case_when(method == "sum" ~ "rowSums",
      method == "mean" ~ "rowMeans")
    data <- mutate_all(data, function(x) as.numeric(as.character(x)))
    eval(parse(text = paste0('result <- ',method,'(data)')))
    data <- data.frame(id = id, data = result)
    names(data) <- c("id",paste0(key,"$",stringr::str_replace_all(field_name," ","_")))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    profile$name <- key
    sep = "\t"
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_HEATMAP"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 2){
      stop("The input data should has at least 2 columns")
    }
    data[is.na(data)] <- 0
    names(data) <- str_replace_all(names(data)," ","_")
    field_names <- names(data)[-1]
    if(length(names(data)) == 2){
      field_tree <- NULL
    }else{
      field_tree <- write.tree(ape::as.phylo(hclust(dist(t(data %>% select(field_names))))))
    }
    names(data) <- c("id",paste0(key,"$",field_names))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    field$labels <- field_names
    if(length(names(data)) == 2){
      specific_themes[["heatmap"]][["tree"]][["tree_display"]] <- 0
    }else{
      specific_themes[["heatmap"]][["tree"]][["tree"]] <- field_tree
    }
    profile$name <- key
    sep = "\t"
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_SYMBOL"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 2){
      stop("The input data should has at least 2 columns")
    }
    if(length(names(data)) > 4){
      stop("The input data should has 2-4 columns: id, type(optional), color(optional), label")
    }
    colname_shape_size = ""
    if(is.null(size_factor)){
      potential_shape_sizes <- names(data)[-1]
      for (potential_shape_size in potential_shape_sizes) {
        if(stringr::str_remove_all(paste0(data[[potential_shape_size]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_shape_size]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as shape size parameter: ", potential_shape_size))
          size_factor = data[[potential_shape_size]]
          colname_shape_size = potential_shape_size
          break
        }
      }
    }
    colname_shape = ""
    if(is.null(shape)){
      potential_shapes <- names(data)[!names(data)%in%c("id", colname_shape_size)]
      for(potential_shape in potential_shapes){
        if(stringr::str_remove_all(paste0(data[[potential_shape]],collapse = ""),"[12345]") == ""){
          message(paste0("Using following column as shape parameter: ", potential_shape))
          shape = data[[potential_shape]]
          colname_shape = potential_shape
          break
        }
      }
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(data[[potential_color]]))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }
    colname_fill = ""
    if(is.null(fill)){
      potential_fills <- names(data)[!names(data)%in%c("id", colname_shape, colname_shape_size)]
      for(potential_fill in potential_fills){
        if(stringr::str_remove_all(paste0(data[[potential_fill]],collapse = ""),"[10-]") == ""){
          message(paste0("Using following column as fill parameter: ", potential_fill))
          fill = data[[potential_fill]]
          colname_fill = potential_fill
          break
        }
      }
    }
    colname_position = ""
    if(is.null(position)){
      potential_positions <- names(data)[!names(data)%in%c("id", colname_shape, colname_shape_size, colname_fill)]
      for (potential_position in potential_positions) {
        if(stringr::str_remove_all(paste0(data[[potential_position]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_position]],collapse = ""),"\\.") <= 1){
          potential_values <- as.numeric(data[[potential_position]])
          if(all((potential_values >= -1) & (potential_values <= 1))){
            message(paste0("Using following column as position parameter: ", potential_position))
            position = data[[potential_position]]
            colname_position = potential_position
            break
          }
        }
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id", colname_shape, colname_color, colname_shape_size, colname_fill,colname_position)]
    if(length(color) != nrow(data)){
      message("Identifying data column to auto setup color parameter")
      if(length(colname_data)!=1){
        stop("Unable to indentify data column")
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
    }
    if(is.null(shape)){
      message("Identifying data column to auto setup shape parameter")
      if(length(unique(data[[colname_data]]))<=5){
        shape_levels = c(1:length(unique(data[[colname_data]])))
        shape = as.factor(data[[colname_data]])
        levels(shape) <- shape_levels
      }else {
         shape = 2
      }
    }
    if(is.null(size_factor)){
      message("The size factor parameter is empty. Using 0.8 as size factor.")
      size_factor = 0.8
    }
    if(is.null(fill)){
      message("The fill parameter is empty. Using 1 as fill.")
      fill = 1
    }
    if(is.null(position)){
      message("The position parameter is empty. Using 0 as position.")
      position = 0
    }
    df_data <- data.frame(id = data[["id"]],symbol = shape, size = size_factor,color = color, fill = fill, position = position)
    names(df_data) <- c("id",paste0(key,c("$SYMBOL", "$SIZE", "$COLOR", "$FILL", "$POSITION")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    common_themes$legend$title <- colname_data
    
    # Fix legend order issue: follow original data frame order, not factor alphabetical order
    # Get unique labels and corresponding shapes, colors, preserving original order
    unique_data <- data.frame(
      label = data[[colname_data]], 
      shape = shape, 
      color = color, 
      stringsAsFactors = FALSE
    )
    unique_data <- unique_data[!duplicated(unique_data$label), ]
    
    common_themes$legend$shapes <- unique_data$shape
    common_themes$legend$colors <- unique_data$color
    common_themes$legend$labels <- unique_data$label
    common_themes$legend$shape_scales <- rep(1,length(unique_data$label))
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_EXTERNALSHAPE"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 2){
      stop("The input data should has at least 2 columns")
    }
    
    # Store original data before any modifications for attribute access
    data_original <- data.table::copy(data)
    
    field_names <- names(data)[-1]
    data <- convert_range_to_node(data, tree)
    names(data) <- c("id",paste0(key,"$",field_names))
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    field$labels <- field_names
    field_length <- length(field_names)
    
    # Identify data column for color assignment
    colname_data <- ""
    if(length(field_names) == 1){
      colname_data <- field_names[1]
    }else{
      remaining_cols <- field_names[!field_names %in% c("id")]
      if(length(remaining_cols) == 1){
        colname_data <- remaining_cols[1]
      }else{
        # Use the first column as default for dual-factor mode
        colname_data <- field_names[1]
      }
    }
    
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    
    # ------------------ Start dual-factor coloring code for DATASET_EXTERNALSHAPE ------------------
    
    # Check for attribute-based grouping (similar to DATASET_BINARY)
    # data_original was stored above
    
    # Determine which attribute to use based on first element of color
    if(length(color) >= 2 && color[1] != "group" && color[1] %in% names(attributes(data_original))){
      color_attr_name <- color[1]
      col_groups <- attr(data_original, color_attr_name)
      has_col_groups <- !is.null(col_groups) && length(col_groups) > 0
    } else if(length(color) >= 2 && color[1] == "group"){
      color_attr_name <- "col_groups"
      col_groups <- attr(data_original, color_attr_name)
      has_col_groups <- !is.null(col_groups) && length(col_groups) > 0
    } else {
      color_attr_name <- "col_groups"
      col_groups <- NULL
      has_col_groups <- FALSE
    }
    
    # When color is specified as two field names, enable dual-factor (main grouping + gradient) strategy
    if((length(color) >= 2 && all(color[1:2] %in% names(data_original))) || has_col_groups){
      color_level_main     <- color[1]
      color_level_gradient <- color[2]
      
      # Optional 3rd element as color palette set
      color_set <- NULL
      if(length(color) >= 3){
        candidate_set <- color[3]
        if(stringr::str_remove(candidate_set,"_.*$") %in% get_color(set = "ls")){
          color_set <- candidate_set
        }
      }
      
      # Check if using attribute-based grouping with separator
      # If color_level_gradient is not a column name, treat it as separator
      is_separator_mode <- has_col_groups && 
        (color_level_main == "group" || color_level_main == color_attr_name) &&
        !(color_level_gradient %in% names(data_original))
      
      if(is_separator_mode){
        # Special case: Using col_groups attribute with separator-based dual-factor coloring
        # Generate colors for each field based on its group membership
        main_groups <- names(col_groups)
        n_main <- length(main_groups)
        
        # Get the separator string from color_level_gradient
        separator <- color_level_gradient
        
        # Generate base colors for main groups
        base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)
        
        # Generate colors for each field based on its group membership
        field$colors <- character(field_length)
        
        for(i in seq_along(field$labels)){
          field_name <- field$labels[i]
          
          # Find which group this field belongs to
          for(j in seq_along(col_groups)){
            if(field_name %in% col_groups[[j]]){
              # This field belongs to group j
              base_color <- base_colors_main[j]
              
              # Get columns in this group
              group_cols <- col_groups[[j]]
              field_index_in_group <- which(group_cols == field_name)
              n_fields_in_group <- length(group_cols)
              
              # Using separator: extract prefix before separator from column names
              if(n_fields_in_group == 1){
                field$colors[i] <- base_color
              } else {
                # Extract prefix before separator using regex
                # Escape special regex characters in separator
                # Fixed approach: manually escape special chars
                if(separator %in% c(".", "|", "(", ")", "+", "$", "*", "?", "[", "]", "{", "}", "^", "\\\\")){
                  sep_escaped <- paste0("\\\\", separator)
                } else {
                  sep_escaped <- separator
                }
                prefix_pattern <- paste0(sep_escaped, ".*$")
                prefixes <- sub(prefix_pattern, "", group_cols)
                unique_prefixes <- unique(prefixes)
                n_unique_prefixes <- length(unique_prefixes)
                
                if(n_unique_prefixes == 1){
                  # All columns have same prefix, use base color
                  field$colors[i] <- base_color
                } else {
                  # Generate gradient colors for unique prefixes
                  gradient_colors <- gradient_color(n_unique_prefixes, base_color)
                  field_prefix <- prefixes[field_index_in_group]
                  prefix_index <- which(unique_prefixes == field_prefix)
                  
                  if(length(prefix_index) > 0 && prefix_index >= 1 && prefix_index <= length(gradient_colors)){
                    field$colors[i] <- gradient_colors[prefix_index]
                  } else {
                    field$colors[i] <- base_color
                  }
                }
              }
              break
            }
          }
        }
        
        # Legend for attribute-based grouping
        common_themes$legend$title <- "Groups"
        common_themes$legend$labels <- field$labels
        common_themes$legend$colors <- as.character(field$colors)
        common_themes$legend$shapes <- rep(1, length(field$labels))
        
      } else {
        # Standard dual-factor coloring using data columns
        main_vec     <- as.factor(data[[color_level_main]])
        gradient_vec <- data[[color_level_gradient]]
    
      # Ensure main factor is sorted alphabetically
      main_levels <- sort(levels(main_vec))
      main_vec <- factor(main_vec, levels = main_levels)
      n_main      <- length(main_levels)
    
      base_colors_main <- if(is.null(color_set)) get_color(n_main) else get_color(n_main, set = color_set)
    
      color_out <- character(nrow(data))
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        for(j in seq_along(idx_main)){
          row_idx  <- idx_main[j]
          grad_val <- grad_vals[j]
          color_out[row_idx] <- grad_color_map[[ as.character(grad_val) ]]
        }
      }
      field$colors <- factor(color_out)
      
      # Legend: for dual-factor, labels use secondary factor name; order by main factor grouping, then alphabetically within groups
      common_themes$legend$title <- color_level_gradient
      legend_labels <- character(0)
      legend_colors <- character(0)
      for(i in seq_len(n_main)){
        this_main  <- main_levels[i]
        base_color <- base_colors_main[i]
        idx_main   <- which(main_vec == this_main)
        grad_vals    <- gradient_vec[idx_main]
        unique_grads <- sort(unique(grad_vals))
        n_grads      <- length(unique_grads)
        colors_grad  <- gradient_color(n_grads, base_color)
        grad_color_map <- setNames(colors_grad, unique_grads)
        legend_labels <- c(legend_labels, as.character(unique_grads))
        legend_colors <- c(legend_colors, as.character(grad_color_map[as.character(unique_grads)]))
      }
      common_themes$legend$labels <- legend_labels
      common_themes$legend$colors <- legend_colors
      common_themes$legend$shapes <- rep(1, length(legend_labels))
      }  # End of standard dual-factor coloring
    } else {
      # Single factor coloring
      if(length(color) == 1 && stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        field$colors <- get_color(field_length,set = color)
      }else{
        field$colors <- get_color(field_length)
      }
      
      # Single factor legend
      if(!is.null(colname_data) && colname_data != "" && colname_data %in% names(data)){
        common_themes$legend$title <- colname_data
        unique_data <- data.frame(label = data[[colname_data]], color = field$colors, stringsAsFactors = FALSE)
        unique_data <- unique_data[!duplicated(unique_data$label), ]
        common_themes$legend$colors <- unique_data$color
        common_themes$legend$labels <- unique_data$label
        common_themes$legend$shapes <- rep(1, length(common_themes$legend$labels))
      } else {
        # Default legend when no valid data column
        common_themes$legend$title <- "Fields"
        common_themes$legend$labels <- field$labels
        common_themes$legend$colors <- as.character(field$colors)
        common_themes$legend$shapes <- rep(1, length(field$labels))
      }
    }
    
    # ------------------- End dual-factor coloring code for DATASET_EXTERNALSHAPE -------------------
    
    # ------------------ Start shape assignment code for DATASET_EXTERNALSHAPE ------------------
    # Handle shape assignment for DATASET_EXTERNALSHAPE
    if(is.null(shape)){
      field$shapes <- rep(1, field_length)
    } else {
      valid_shapes <- c("RE","HH","HV","EL","DI","TR","TL","PL","PR","PU","PD","OC","GP")
      if(shape %in% valid_shapes){
        field$shapes <- rep(shape, field_length)
      } else if(shape == "group" || (shape %in% names(attributes(data_original)))){
        # Check if shape is "group" (default) or a custom attribute name (e.g., "col_groups_shape")
        shape_attr_name <- if(shape == "group") "col_groups" else shape
        col_groups <- attr(data_original, shape_attr_name)
        
        if(!is.null(col_groups) && length(col_groups) > 0){
          # Assign shapes based on group membership
          field$shapes <- rep(1, field_length)  # Default shape
          
          n_groups <- length(col_groups)
          available_shapes <- c(1, 2, 3, 4, 5, 6)  # iTOL supports shapes 1-6 for EXTERNALSHAPE
          
          for(i in seq_along(col_groups)){
            group_name <- names(col_groups)[i]
            group_cols <- col_groups[[i]]
            shape_idx <- available_shapes[(i - 1) %% length(available_shapes) + 1]
            
            # Find field indices that belong to this group
            for(j in seq_along(field$labels)){
              if(field$labels[j] %in% group_cols){
                field$shapes[j] <- shape_idx
              }
            }
          }
        } else {
          # No col_groups attribute, use default
          field$shapes <- rep(1, field_length)
        }
      } else if(is.numeric(shape)){
        # If shape is a numeric value (1-6 for iTOL shape types)
        field$shapes <- rep(shape, field_length)
      } else {
        # Default fallback
        field$shapes <- rep(1, field_length)
      }
    }
    
    # Update legend shapes to match field shapes
    if(length(common_themes$legend$shapes) == length(field$shapes)){
      common_themes$legend$shapes <- field$shapes
    }
    # ------------------ End shape assignment code for DATASET_EXTERNALSHAPE ------------------
    
    profile$name <- key
    sep = "\t"
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_DOMAINS"){
    shape_by = NULL
    if(length(names(data)) == 3){
      if(is.null(shape)){
        data <- data[order(data[[2]],data[[3]]),]
        shape_by <- factor(pull(data[,2]),levels = unique(pull(data[,2])))
        levels(shape_by) <- c("RE","HH","HV","EL","DI","TR","TL","PL","PR","PU","PD","OC","GP")[1:length(levels(shape_by))]
        shape = shape_by
        data <- data[,-2]
        data[,2] <- factor(pull(data[,2]),levels = unique(pull(data[,2])))
      }else{
        data <- data[order(data[[2]],data[[3]]),]
        color_by <- factor(pull(data[,2]),levels = unique(pull(data[,2])))
        if(color %in% get_color(set="ls")){
          levels(color_by) <- get_color(n=length(levels(color_by)),set = color)
        }else{
          stop("The color parameter should be within get_color sets.")
        }
        data_with_colors <- data.frame()
        for (i in 1:length(levels(color_by))) {
          sub_data <- data[which(data[,2]==unique(pull(data[,2]))[i]),]
          n = length(unique(pull(sub_data[,3])))
          sub_color_by <- gradient_color(n,levels(color_by)[i])
          sub_data$colors <- factor(pull(sub_data[,3]),levels = unique(pull(sub_data[,3])))
          levels(sub_data$colors) <- sub_color_by
          sub_data <- sub_data[,-2]
          sub_data[,2] <- factor(pull(sub_data[,2]),levels = unique(pull(sub_data[,2])))
          data_with_colors <- rbind(data_with_colors,sub_data)
        }
        data <- data_with_colors
        color = data$colors
      }
    }
    if(length(names(data)) %in% c(2,3)){
      data <- data.frame(data,length=rep(10,nrow(data)),start=rep(0,nrow(data)),end=rep(10,nrow(data)))
    }
    length = NULL
    start = NULL
    end = NULL
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 5){
      stop("The input data should has at least 5 columns")
    }
    colname_length = ""
    if(is.null(length)){
      potential_lengths <- names(data)[-1]
      for (potential_length in potential_lengths) {
        if(stringr::str_remove_all(paste0(data[[potential_length]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_length]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as length parameter: ", potential_length))
          length = data[[potential_length]]
          colname_length = potential_length
          break
        }
      }
    }
    colname_shape = ""
    if(is.null(shape)){
      potential_shapes <- names(data)[-1]
      for(potential_shape in potential_shapes){
        if(stringr::str_remove_all(paste0(data[[potential_shape]],collapse = ""),"RE|HH|HV|EL|DI|TR|TL|PL|PR|PU|PD|OC|GP") == ""){
          message(paste0("Using following column as shape parameter: ", potential_shape))
          shape = data[[potential_shape]]
          colname_shape = potential_shape
          break
        }
      }
    }
    colname_start = ""
    if(is.null(start)){
      potential_starts <- names(data)[!names(data)%in%c("id", colname_length)]
      for (potential_start in potential_starts) {
        if(stringr::str_remove_all(paste0(data[[potential_start]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_start]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as start parameter: ", potential_start))
          start = data[[potential_start]]
          colname_start = potential_start
          break
        }
      }
    }
    colname_end = ""
    if(is.null(end)){
      potential_ends <- names(data)[!names(data)%in%c("id", colname_length, colname_start)]
      for (potential_end in potential_ends) {
        if(stringr::str_remove_all(paste0(data[[potential_end]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_end]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as end parameter: ", potential_end))
          end = data[[potential_end]]
          colname_end = potential_end
          break
        }
      }
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(as.character(data[[potential_color]])))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }else{
      if("colors"%in%names(data)){
        colname_color = "colors"
        color = data$colors
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id", colname_length, colname_shape, colname_start, colname_end, colname_color)]
    if(length(color) != nrow(data)){
      message("Identifying data column to auto setup color parameter")
      if(length(colname_data)!=1){
        #stop("Unable to indentify data column")
      }
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
    }
    if(is.null(shape)){
      message("Identifying data column to auto setup shape parameter")
      if(length(unique(data[[colname_data]]))<=13){
        shape_levels = c("RE","HH","HV","EL","DI","TR","TL","PL","PR","PU","PD","OC","GP")[1:length(unique(data[[colname_data]]))]
        shape = as.factor(data[[colname_data]])
        levels(shape) <- shape_levels
      }else {
         shape = "RE"
      }
    }
    df_data <- data.frame(id = data[["id"]],length = length, shape = shape, start = start, end = end, color = color, label = data[[colname_data]])
    names(df_data) <- c("id",paste0(key,c("$LENGTH", "$SHAPE", "$START", "$END", "$COLOR", "$LABEL")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    if(is.null(shape_by)){
      # Fix legend order issue: follow original data frame order, not factor alphabetical order
      # Get unique labels and corresponding shapes, colors, preserving original order
      unique_data <- data.frame(
        label = data[[colname_data]], 
        shape = shape, 
        color = color, 
        stringsAsFactors = FALSE
      )
      unique_data <- unique_data[!duplicated(unique_data$label), ]
      
      if(length(unique(unique_data$shape))==1){
        common_themes$legend$shapes <- rep(unique(unique_data$shape), length(unique(unique_data$color)))
      }else {
         common_themes$legend$shapes <- unique_data$shape
      }
      common_themes$legend$colors <- unique_data$color
      common_themes$legend$labels <- unique_data$label
    }else {
       common_themes$legend$shapes <- shape_by[!duplicated(data[[colname_data]])]
       # For shape_by cases, also need to fix color and label order
       unique_data <- data.frame(
         label = data[[colname_data]], 
         color = color, 
         stringsAsFactors = FALSE
       )
       unique_data <- unique_data[!duplicated(unique_data$label), ]
       common_themes$legend$colors <- unique_data$color
       common_themes$legend$labels <- unique_data$label
    }
           # Labels have already been set above, no need to repeat here
    specific_themes$basic_plot$dataset_scale <- start %>% unique() %>% sort()
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_SIMPLEBAR"){
    if(data.table::is.data.table(data)){
      data <- as.data.frame(data)
    }
    if(!is.data.frame(data)){
      stop("The input data class should be a data.frame or data.table")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(is.null(method)){
      method = "sum"
    }else {
       if(!method %in% c("sum","mean")){
        warning("Unsupported method. Using sum as method parameter")
        method = "sum"
       }
    }
    if(length(names(data)) > 2){
      message("The input data are mutate as one column by method: ", method)
    }
    field_name = names(data)[2]
    if(length(names(data))>2){
      field_name <- paste0(method," of ", paste(names(data)[-1],collapse = ", "))
    }
    id <- data[["id"]]
    data <- data[,-1, drop = FALSE]
    data <- mutate_all(data, function(x) as.numeric(as.character(x)))
    min <- min(data)
    mean <- data %>% rowMeans() %>% mean()
    max <- max(data)
    data[is.na(data)] <- 0
    method = case_when(method == "sum" ~ "rowSums",
      method == "mean" ~ "rowMeans")
    eval(parse(text = paste0('result <- ',method,'(data)')))
    data <- data.frame(id = id, data = result)
    names(data) <- c("id",paste0(key,"$",stringr::str_replace_all(field_name," ","_")))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    profile$name <- key
    sep = "\t"
    specific_themes$basic_plot$dataset_scale <- c(min,mean,max)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_MULTIBAR"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(length(names(data)) < 2){
      stop("The input data should has at least 2 columns")
    }
    field_names <- names(data)[-1]
    names(data) <- c("id",paste0(key,"$",field_names))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    field_length <- length(field_names)
    field$labels <- field_names
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
      field$colors <- get_color(field_length,set = color)
    }else{
      field$colors <- get_color(field_length)
    }
    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type=="DATASET_BOXPLOT"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(names(data)[2] != "x"){
        message(paste0("Using the second column as x: ",names(data)[2]))
        names(data)[2] <- "x"
    }
    n = length(unique(data$id))
    min <- min(data$x)
    mean <- mean(data$x)
    max <- max(data$x)
    df_data <- data.frame(id="",min="",q1="",mid="",q3="",max="",ev="")
    for (i in 1:n) {
      stat <- boxplot.stats(data$x[which(data$id==unique(data$id)[i])])
      df_data[i,]$id = unique(data$id)[i]
      df_data[i,]$min = stat$stats[1]
      df_data[i,]$q1 = stat$stats[2]
      df_data[i,]$mid = stat$stats[3]
      df_data[i,]$q3 = stat$stats[4]
      df_data[i,]$max = stat$stats[5]
      df_data[i,]$ev = paste0(stat$out,collapse = ",")
    }
    names(df_data) <- c("id",paste0(key,c("$MINIMUM", "$Q1", "$MEDIAN", "$Q3", "$MAXIMUM", "$EXTREME_VALUES")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    sep = "\t"
    specific_themes$basic_plot$dataset_scale <- c(min,mean,max)
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_LINECHART"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the 1st column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(names(data)[2] != "X"){
        message(paste0("Using the 2nd column as X: ",names(data)[2]))
        names(data)[2] <- "X"
    }
    if(names(data)[3] != "Y"){
        message(paste0("Using the 3rd column as Y: ",names(data)[3]))
        names(data)[3] <- "Y"
    }
    if(length(names(data)) != 3){
      stop("The input data should has 3 columns: id, x, y")
    }
    names(data) <- c("id",paste0(key,c("$X", "$Y")))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    profile$name <- key
    specific_themes$linechart$basic$x <- ""
    specific_themes$linechart$basic$y <- ""
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_PIECHART"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the 1st column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    
    # Handle single column case (e.g., Completeness as percentage or proportion)
    if(ncol(data) == 2){
      col_name <- names(data)[2]
      values <- data[,2]
      
      # Auto-detect data range and calculate appropriate remainder
      max_val <- max(values, na.rm = TRUE)
      min_val <- min(values, na.rm = TRUE)
      
      if(max_val <= 1 && min_val >= 0){
        # Data is in 0-1 range (proportion)
        message("Single column detected. Data range 0-1, creating pie chart with value and remainder (1 - value)")
        remainder <- 1 - values
        remainder_label <- "Remainder"
      } else if(max_val <= 100 && min_val >= 0){
        # Data is in 0-100 range (percentage)
        message("Single column detected. Data range 0-100, creating pie chart with value and remainder (100 - value)")
        remainder <- 100 - values
        remainder_label <- "Remainder"
      } else {
        # Default to percentage range if data doesn't fit standard ranges
        message("Single column detected. Data range not standard, defaulting to percentage calculation (100 - value)")
        remainder <- 100 - values
        remainder_label <- "Remainder"
      }
      
      # Create a data frame with value and remainder
      data <- data.frame(
        id = data[,1],
        POSITION = if(is.null(position)) 0 else position,
        RADIUS = if(is.null(size_factor)) 1 else size_factor,
        Value = values,
        Remainder = remainder,
        stringsAsFactors = FALSE
      )
      field_names <- c("Value", remainder_label)
    } else {
      # Original multi-column logic
      if(is.null(position)){
        message(paste0("Using the 2nd column as position: ",names(data)[2]))
        names(data)[2] <- "POSITION"
      }else{
        data <- data.frame(data[,1],POSITION=position,data[,2:ncol(data)])
      }
      if(is.null(size_factor)){
        message(paste0("Using the 3rd column as radius: ",names(data)[3]))
        names(data)[3] <- "RADIUS"
      }else {
         data <- data.frame(data[,1:2],RADIUS=size_factor,data[,3:ncol(data)])
      }
      field_names <- names(data)[-c(1:3)]
    }
    
    names(data) <- c("id",paste0(key,"$",c("POSITION", "RADIUS",field_names)))
    data <- convert_range_to_node(data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], data)
    field_length <- length(field_names)
    field$labels <- field_names
    
    # Handle color assignment with minimum length of 1
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
      field$colors <- get_color(max(field_length, 1), set = color)
    }else{
      field$colors <- get_color(max(field_length, 1))
    }
    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type == "DATASET_ALIGNMENT"){
    if(!is.data.frame(data)){
      if(file.exists(data)){
        df_data <- fa_read(data)
        names(df_data) <- c("id",paste0(key,c("$SEQUENCE")))
      }
      stop("The input data class should be a data frame or a alignment file")
    }else {
      if(length(names(data)) > 2){
        message("The input data should be 2 column: id, seq")
      }
      df_data <- data[,1:2]
      df_data[,1] <- as.character(df_data[,1])
      names(df_data) <- c("id",paste0(key,c("$SEQUENCE")))
    }
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    specific_themes$alignment$reference$ids <- 0
    specific_themes$alignment$reference$use <- 0
    specific_themes$alignment$reference$box_border_width <- 0
    specific_themes$alignment$reference$box_border_color <- ""
    specific_themes$alignment$reference$box_fill_color <- ""
    specific_themes$alignment$highlight$type <- ""
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type=="DATASET_CONNECTION"){
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
        message(paste0("Using the first column as id: ",names(data)[1]))
        names(data)[1] <- "id"
    }
    if(names(data)[2] != "target"){
        message(paste0("Using the second column as target: ",names(data)[2]))
        names(data)[2] <- "target"
    }
    colname_size_factor = ""
    if(is.null(size_factor)){
      potential_size_factors <- names(data)[-1]
      for (potential_size_factor in potential_size_factors) {
        if(stringr::str_remove_all(paste0(data[[potential_size_factor]],collapse = ""),"[\\d\\.]") == ""){
          message(paste0("Using following column as size factor parameter: ", potential_size_factor))
          size_factor = data[[potential_size_factor]]
          colname_size_factor = potential_size_factor
        }
      }
    }
    colname_color = ""
    if(is.null(color)){
      potential_colors <- names(data)[-1]
      for(potential_color in potential_colors){
        if(min(stringr::str_length(data[[potential_color]]))>3){
          str_1 <- unique(stringr::str_extract(data[[potential_color]],"^."))
          str_2 <- unique(stringr::str_extract(data[[potential_color]],"^..."))
          if(length(str_1)==1){
            if(str_1 == "#"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
          if(length(str_2)==1){
            if(str_2 == "rgb"){
              message(paste0("Using following column as color parameter: ", potential_color))
              color = data[[potential_color]]
              colname_color = potential_color
            }
          }
        }
      }
    }
    colname_line_type = ""
    if(is.null(line_type)){
      potential_line_types <- names(data)[-1]
      for(potential_line_type in potential_line_types){
        if(all(unique(data[[potential_line_type]]) %in% c("normal","dashed"))){
          message(paste0("Using following column as line type parameter: ", potential_line_type))
          line_type = data[[potential_line_type]]
          colname_line_type = potential_line_type
        }
      }
    }
    colname_data <- names(data)[!names(data)%in%c("id","target", colname_size_factor, colname_color, colname_line_type)]
    if(length(color) != nrow(data)){
      message("Using data column to auto setup color parameter")
    }
    if(is.null(color)){
      message("Using default color pattern: table2itol")
      color = "table2itol"
    }
    if(length(color) == 1){
      if(stringr::str_remove(color,"_.*$") %in% get_color(set="ls")){
        color_levels = get_color(length(unique(data[[colname_data]])),set = color)
        color = as.factor(data[[colname_data]])
        levels(color) <- color_levels
      }else {
        if(stringr::str_detect(color,"^#")||stringr::str_detect(color,"^rgb")){
          color = rep(color,nrow(data))
        }else{
          stop("Unsupported color parameter")
        }
      }
    }
    if(is.null(size_factor)){
      message("The size factor parameter is empty. Using 0 as position.")
      size_factor = 1
    }
    if(is.null(line_type)){
      message("The line type  parameter is empty. Using 0 as position.")
      line_type = "normal"
    }
    df_data <- data.frame(id = data[["id"]], target = data[["target"]], width = size_factor, color = color, style = line_type, label = data[[colname_data]])
    names(df_data) <- c("id",paste0(key,c("$TARGET","$WIDTH","$COLOR","$STYLE","$LABEL")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    sep = "\t"
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type =="DATASET_IMAGE"){#ID,position,size_factor,rotation,horizontal_shift,vertical_shift,image_url
    horizontal_shift = NULL
    vertical_shift = NULL
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the first column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    if(length(names(data)) > 7 ){
      stop("The input data should has 2-7 columns: id, position(optional), size factor(optional), rotation(optional), horizontal shift(optional), vertical shift(optional), image url")
    }
    colname_position = ""
    if(is.null(position)){
      potential_positions <- names(data)[-1]
      for (potential_position in potential_positions) {
        if(stringr::str_remove_all(paste0(data[[potential_position]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_position]],collapse = ""),"\\.") <= 1){
          potential_values <- as.numeric(data[[potential_position]])
          if(all((potential_values >= -1) & (potential_values <= 1))){
            message(paste0("Using following column as position parameter: ", potential_position))
            position = data[[potential_position]]
            colname_position = potential_position
            break
          }
        }
      }
    }
    colname_image_size = ""
    if(is.null(size_factor)){
      potential_image_sizes <- names(data)[!names(data)%in%c("id", colname_position)]
      for (potential_image_size in potential_image_sizes) {
        if(stringr::str_remove_all(paste0(data[[potential_image_size]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_image_size]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as image size parameter: ", potential_image_size))
          size_factor = data[[potential_image_size]]
          colname_image_size = potential_image_size
          break
        }
      }
    }
    colname_rotation = ""
    if(is.null(rotation)){
      potential_rotations <- names(data)[!names(data)%in%c("id", colname_position, colname_image_size)]
      for (potential_rotation in potential_rotations) {
        if(stringr::str_remove_all(paste0(data[[potential_rotation]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_rotation]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as rotation parameter: ", potential_rotation))
          rotation = data[[potential_rotation]]
          colname_rotation = potential_rotation
          break
        }
      }
    }
    colname_horizontal_shift = ""
    if(is.null(horizontal_shift)){
      potential_horizontal_shifts <- names(data)[!names(data)%in%c("id", colname_position, colname_image_size, colname_rotation)]
      for (potential_horizontal_shift in potential_horizontal_shifts) {
        if(stringr::str_remove_all(paste0(data[[potential_horizontal_shift]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_horizontal_shift]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as horizontal_shift parameter: ", potential_horizontal_shift))
          horizontal_shift = data[[potential_horizontal_shift]]
          colname_horizontal_shift = potential_horizontal_shift
          break
        }
      }
    }
    colname_vertical_shift = ""
    if(is.null(vertical_shift)){
      potential_vertical_shifts <- names(data)[!names(data)%in%c("id", colname_position, colname_image_size, colname_rotation, colname_horizontal_shift)]
      for (potential_vertical_shift in potential_vertical_shifts) {
        if(stringr::str_remove_all(paste0(data[[potential_vertical_shift]],collapse = ""),"[\\d\\.-]") == "" && stringr::str_count(paste0(data[[potential_vertical_shift]],collapse = ""),"\\.") <= 1){
          message(paste0("Using following column as vertical_shift parameter: ", potential_vertical_shift))
          vertical_shift = data[[potential_vertical_shift]]
          colname_vertical_shift = potential_vertical_shift
          break
        }
      }
    }
    colname_image_url <- names(data)[!names(data)%in%c("id", colname_position, colname_image_size, colname_rotation, colname_horizontal_shift, colname_vertical_shift)]
    if(is.null(position)){
      message("The position parameter is empty. Using 0 as position.")
      position = 0
    }
    if (is.null(size_factor)) {
      message("The image size parameter is empty. Using 1 as image size.")
      size_factor = 1
    }
    if(is.null(rotation)){
      message("The rotation parameter is empty. Using 0 as rotation.")
      rotation = 0
    }
    if (is.null(horizontal_shift)) {
      message("The horizontal_shift parameter is empty. Using 0.")
      horizontal_shift = "0"
    }
    if (is.null(vertical_shift)) {
      message("The vertical_shift parameter is empty. Using 0.")
      vertical_shift = "0"
    }
    df_data <- data.frame(id = data[["id"]], position = position, size_factor = size_factor, rotation = rotation, horizontal_shift = horizontal_shift, vertical_shift = vertical_shift, image_url = data[[colname_image_url]])
    names(df_data) <- c("id",paste0(key,"$",c("POSITION", "SIZE_FACTOR", "ROTATION", "HORIZONTAL_SHIFT", "VERTICAL_SHIFT", "IMAGE_URL")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  if(type=="POPUP_INFO"){#NODE_ID,POPUP_TITLE,POPUP_CONTENT
    if(!is.data.frame(data)){
      stop("The input data class should be a data frame")
    }
    if(names(data)[1] != "id"){
      message(paste0("Using the 1st column as id: ",names(data)[1]))
      names(data)[1] <- "id"
    }
    if(length(names(data)) != 3 ){
      stop("The input data should has 3 columns: id, title, content")
    }
    if(names(data)[2] != "title"){
        message(paste0("Using the 2nd column as title: ",names(data)[2]))
        names(data)[2] <- "title"
    }
    if(names(data)[3] != "content"){
        message(paste0("Using the 3rd column as content: ",names(data)[3]))
        names(data)[3] <- "content"
    }
    df_data <- data.frame(id = data[["id"]], title = data[["title"]], content = data[["content"]])
    names(df_data) <- c("id",paste0(key,"$",c("POPUP_TITLE", "POPUP_CONTENT")))
    df_data <- convert_range_to_node(df_data, tree)
    data_left[["node"]] <- df_merge(data_left[["node"]], df_data)
    data_left[["tip"]] <- df_merge(data_left[["tip"]], df_data)
    profile$name <- key
    unit <- new("itol.unit", type = type, sep = sep, profile = profile, field = field, common_themes = common_themes, specific_themes = specific_themes, data = data_left)
  }
  return(unit)
}

#' Calculate tree based on count matrix
#' @description While we start analysis from count matrix not sequences
#' alignment, we could use clustering methods to get main tree in phylo object
#' class of output as Newick format file. If the samples or elements have group
#' information, we could use weighted clustering method to get a clear grouped
#' structure.
#' @param count a data frame containing numberic values of abundance or other
#' count.
#' @param group a vector of character containing the group information. The
#' length of the vector should be same with the count columns number. If using
#' unweighted clustring, should ignore this parameter.
#' @param weight a number specifying the weight size of the group information.
#' In most case, 1 is enough. If the value is between 0 and 1, it will make the
#' weight of group information weak. If the value is more than 1, it will make
#' the weight of group information strong.
#' @return a phylo class object containing
#' @return \item{edge}{a vector of integers specifying edge id. The length of
#' vector is double of node number}
#' @return \item{edge.length}{a vector of numbers specifying edge length}
#' @return \item{tip.label}{a vector of character specifying the tip label}
#' @return \item{Nnode}{a number specifying the number of nodes}
#' @return \item{node.label}{a vector of character specifying the node label.
#' If the tree calculated from count matrix or other case, the node label will
#' generated by ape::makeNodeLabel function. And the Most Recent Common
#' Ancestors(MRCA) node will be named with weighted group information, if the
#' parameter group is not null.}
#'
#' @importFrom dplyr mutate filter pull
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom ape as.phylo
#' @importFrom ape makeNodeLabel
#' @importFrom ape mrca
#' @export
count_to_tree <- function(count,group=NULL,weight=0){
  if(!is.null(group)){
    n = as.integer(length(count) * weight)
    if(n <= 0){
      stop("weight size must > 0 while using weighted clustring")
    }
    for (i in 1:n) {
      count = count %>%
        mutate(!!paste0("G",i) := as.numeric(factor(group)))
    }
  }
  dist <- dist(count)
  clust <- hclust(dist)
  tree <- as.phylo(clust)
  if (is.null(tree$node.label)) {
    tree <- ape::makeNodeLabel(phy = tree, method = "number", prefix = "I")
  }
  if(!is.null(group)){
    mrca <- ape::mrca(tree)
    group_names <- unique(group)
    group_number <- length(group_names)
    group_templates_mrca_min_node_ids <- c()
    for (i in 1:group_number) {
      group_templates <- template_groups %>%
        filter(group == group_names[i]) %>%
        pull(template)
      group_templates_mrca <- mrca[group_templates,group_templates]
      group_templates_mrca_values <- group_templates_mrca[row(group_templates_mrca) == (col(group_templates_mrca) - 1)] # min without diagonal
      group_templates_mrca_min <- min(group_templates_mrca_values)
      group_templates_mrca_min_node_ids[i] <- group_templates_mrca_min - tree$Nnode - 1
    }
    tree$node.label[group_templates_mrca_min_node_ids] <- group_names
  }
  return(tree)
}


utils::globalVariables(c("inbuilt_themes",":=","tempalte_groups","template","result", "template_groups"))
