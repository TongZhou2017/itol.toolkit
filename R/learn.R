## Learn Data

#' Learn template type
#'
#' @description Extract first line of template to learn type information.
#' @param file template file
#' @export
learn_type <- function(file) {
  lines <- readLines(file)
  type <- lines[1]
  return(type)
}

#' Learn from tree
#'
#' @description Learn initial data frame from Newick format tree leaves.
#' @param tree Newick tree file
#' @param node output node label
#' @param tip output tip label
#' @importFrom ape read.tree
#' @export
learn_df <- function(tree, node = FALSE, tip = TRUE) {
  if (is.character(tree)) {
    tree <- read.tree(tree)
  }
  node_label <- as.character(tree$node.label)
  tip_label <- as.character(tree$tip.label)
  if (node) {
    if (tip) {
      df <- list(node = data.frame(id = node_label),
                 tip = data.frame(id = tip_label))
    } else {
      df <- list(node = data.frame(id = node_label))
    }
  } else {
    (
      if (tip) {
        df <- list(tip = data.frame(id = tip_label))
      } else {
        df <- NULL
      })
  }
  return(df)
}

`%+%` <- function(df1, df2) {

}

merge_df <- function(df1, df2) {

}

#' Filter out comments and empty lines
#'
#' @description Remove the lines start with # or without any information.
#' @param lines string vector
#' @param file template file
#' @export
line_clean <- function(lines = NULL, file = NULL) {
  if (!is.null(file)) {
    lines <- readLines(file)
  }
  lines_clean <- lines[!grepl("^#", lines)]
  lines_clean <- lines_clean[!grepl("^$", lines_clean)]
  return(lines_clean)
}

###################################################
##                                               ##
##                 LEARN THEME                   ##
##                                               ##
###################################################

#' Learn separator
#'
#' @description Learn 3 types of separators: tab, space, and comma.
#' @param lines string vector
#' @param file template file
#' @importFrom stringr str_remove
#' @importFrom dplyr case_when
#' @export
learn_separator <- function(lines = NULL, file = NULL) {
  if (!is.null(file)) {
    lines <- line_clean(file = file)
  }
  separator <- lines[grepl("^SEPARATOR", lines)]
  separator <- stringr::str_remove(separator, "^SEPARATOR[ \t,]")
  separator <- case_when(
    separator == "TAB" ~ "\t",
    separator == "SPACE" ~ " ",
    separator == "COMMA" ~ ","
  )
  return(separator)
}

#' Learn paramter
#'
#' @description learn paramter name and values based on the key name in the
#' front of line.
#' @param lines file lines
#' @param param a charactor string of paramter key name
#' @param sep separator
#' @importFrom stringr str_remove
#' @export
learn_line <- function(lines, param, sep) {
  string <- lines[grepl(paste0("^", param, sep), lines)]
  string <- stringr::str_remove(string, paste0("^", param, sep))
  string <- unlist(strsplit(string, sep))
  return(string)
}

#' Learn legend
#'
#' @description learn legend paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_legend <- function(lines, sep) {
  legend_title <- learn_line(lines, "LEGEND_TITLE", sep)
  legend_position_x <- learn_line(lines, "LEGEND_POSITION_X", sep)
  legend_position_y <- learn_line(lines, "LEGEND_POSITION_Y", sep)
  legend_horizontal <- learn_line(lines, "LEGEND_HORIZONTAL", sep)
  legend_shapes <- learn_line(lines, "LEGEND_SHAPES", sep)
  legend_colors <- learn_line(lines, "LEGEND_COLORS", sep)
  legend_labels <- learn_line(lines, "LEGEND_LABELS", sep)
  legend_shape_scales <- learn_line(lines, "LEGEND_SHAPE_SCALES", sep)
  legend <- list(
    title = legend_title,
    position_x = legend_position_x,
    position_y = legend_position_y,
    horizontal = legend_horizontal,
    shapes = legend_shapes,
    colors = legend_colors,
    labels = legend_labels,
    shape_scales = legend_shape_scales
  )
  if (is.null(unlist(legend))) {
    legend <- NULL
  }
  return(legend)
}

#' Learn strip label
#'
#' @description learn strip label paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_strip_label <- function(lines, sep) {
  strip_labels_display <- learn_line(lines, "SHOW_STRIP_LABELS", sep)
  strip_label_width <- learn_line(lines, "STRIP_WIDTH", sep)
  strip_label_size <- learn_line(lines, "STRIP_LABEL_SIZE_FACTOR", sep)
  strip_label_color <- learn_line(lines, "STRIP_LABEL_COLOR", sep)
  strip_label_color_branches <- learn_line(lines, "COLOR_BRANCHES", sep)
  strip_label_position <- learn_line(lines, "STRIP_LABEL_POSITION", sep)
  strip_label_shift <- learn_line(lines, "STRIP_LABEL_SHIFT", sep)
  strip_label_rotation <- learn_line(lines, "STRIP_LABEL_ROTATION", sep)
  strip_label_outline_width <- learn_line(lines, "STRIP_LABEL_OUTLINE", sep)
  strip_label <- list(
    display = strip_labels_display,
    width = strip_label_width,
    size = strip_label_size,
    color = strip_label_color,
    color_branches = strip_label_color_branches,
    position = strip_label_position,
    shift = strip_label_shift,
    rotation = strip_label_rotation,
    outline_width = strip_label_outline_width
  )
  if (is.null(unlist(strip_label))) {
    strip_label <- NULL
  }
  return(strip_label)
}

#' Learn field
#'
#' @description learn field paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_field <- function(lines, sep) {
  field_shapes <- learn_line(lines, "FIELD_SHAPES", sep)
  field_labels <- learn_line(lines, "FIELD_LABELS", sep)
  field_colors <- learn_line(lines, "FIELD_COLORS", sep)
  field <- list(
    labels = field_labels,
    colors = field_colors,
    shapes = field_shapes
  )
  if (is.null(unlist(field))) {
    field <- NULL
  }
  return(field)
}

#' Learn profile
#'
#' @description learn profile paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_profile <- function(lines, sep) {
  dataset_name <- learn_line(lines, "DATASET_LABEL", sep)
  dataset_name <- sub(" ", "_", dataset_name)
  dataset_color <- learn_line(lines, "COLOR", sep)
  profile <- list(
    name = dataset_name,
    color = dataset_color
  )
  if (is.null(unlist(profile))) {
    profile <- NULL
  }
  return(profile)
}

#' Learn label
#'
#' @description learn label paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_label <- function(lines, sep) {
  label_display <- learn_line(lines, "SHOW_LABELS", sep)
  label_size_factor <- learn_line(lines, "LABEL_SIZE_FACTOR", sep)
  label_on_top <- learn_line(lines, "LABELS_ON_TOP", sep)
  label_below <- learn_line(lines, "LABELS_BELOW", sep)
  label_rotation <- learn_line(lines, "LABEL_ROTATION", sep)
  label_straight <- learn_line(lines, "STRAIGHT_LABELS", sep)
  label_vertical <- learn_line(lines, "VERTICAL_SHIFT", sep)
  label_shift <- learn_line(lines, "LABEL_SHIFT ", sep)
  label_external_shift <- learn_line(lines, "EXTERNAL_LABEL_SHIFT", sep)
  label <- list(
    display = label_display,
    size = label_size_factor,
    top = label_on_top,
    below = label_below,
    rotation = label_rotation,
    straight = label_straight,
    vertical = label_vertical,
    shift = label_shift,
    external_shift = label_external_shift
  )
  if (is.null(unlist(label))) {
    label <- NULL
  }
  return(label)
}

#' Learn bar
#'
#' @description learn bar paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_bar <- function(lines, sep) {
  bar_shift <- learn_line(lines, "BAR_SHIFT", sep)
  bar_zero <- learn_line(lines, "BAR_ZERO", sep)
  bar <- list(
    shift = bar_shift,
    zero = bar_zero
  )
  if (is.null(unlist(bar))) {
    bar <- NULL
  }
  return(bar)
}

#' Learn heatmap
#'
#' @description learn heatmap paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_heatmap <- function(lines, sep) {
  heatmap_color_nan <- learn_line(lines, "COLOR_NAN", sep)
  heatmap_color_min <- learn_line(lines, "COLOR_MIN", sep)
  heatmap_color_mid <- learn_line(lines, "COLOR_MID", sep)
  heatmap_color_max <- learn_line(lines, "COLOR_MAX", sep)
  heatmap_user_min_value <- learn_line(lines, "USER_MIN_VALUE", sep)
  heatmap_user_mid_value <- learn_line(lines, "USER_MID_VALUE", sep)
  heatmap_user_max_value <- learn_line(lines, "USER_MAX_VALUE", sep)
  field_tree <- learn_line(lines, "FIELD_TREE", sep)
  field_tree_display <- learn_line(lines, "SHOW_TREE", sep)
  heatmap_use_color_mid <- learn_line(lines, "USE_MID_COLOR", sep)
  heatmap_auto_legend <- learn_line(lines, "AUTO_LEGEND", sep)
  heatmap <- list(
    color = list(
      nan = heatmap_color_nan,
      min = heatmap_color_min,
      mid = heatmap_color_mid,
      max = heatmap_color_max
    ),
    value = list(
      min = heatmap_user_min_value,
      mid = heatmap_user_mid_value,
      max = heatmap_user_max_value
    ),
    tree = list(
      tree = field_tree,
      tree_display = field_tree_display
    ),
    use_mid = heatmap_use_color_mid,
    auto_legend = heatmap_auto_legend
  )
  if (is.null(unlist(heatmap))) {
    heatmap <- NULL
  }
  return(heatmap)
}

#' Learn border
#'
#' @description learn border paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_border <- function(lines, sep) {
  border_width <- learn_line(lines, "BORDER_WIDTH", sep)
  border_color <- learn_line(lines, "BORDER_COLOR", sep)
  border_complete <- learn_line(lines, "COMPLETE_BORDER", sep)
  border <- list(
    width = border_width,
    color = border_color,
    complete = border_complete
  )
  if (is.null(unlist(border))) {
    border <- NULL
  }
  return(border)
}

#' Learn domain
#'
#' @description learn domain paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_domain <- function(lines, sep) {
  domain_labels_display <- learn_line(lines, "SHOW_DOMAIN_LABELS", sep)
  domain_gradient_fill <- learn_line(lines, "GRADIENT_FILL", sep)
  domain_backbone_color <- learn_line(lines, "BACKBONE_COLOR", sep)
  domain_backbone_height <- learn_line(lines, "BACKBONE_HEIGHT", sep)
  domain <- list(
    label_display = domain_labels_display,
    gradient_fill = domain_gradient_fill,
    backbone = list(
      color = domain_backbone_color,
      height = domain_backbone_height
    )
  )
  if (is.null(unlist(domain))) {
    domain <- NULL
  }
  return(domain)
}

#' Learn linechart
#'
#' @description learn linechart paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_linechart <- function(lines, sep) {
  linechart_colors <- learn_line(lines, "LINE_COLORS", sep)
  linechart_x <- learn_line(lines, "AXIS_X", sep)
  linechart_y <- learn_line(lines, "AXIS_Y", sep)
  linechart_vertical <- learn_line(lines, "VERTICAL_CHART", sep)
  linechart_shift <- learn_line(lines, "CHART_SHIFT", sep)
  linechart_line_display <- learn_line(lines, "SHOW_LINE", sep)
  linechart_line_width <- learn_line(lines, "LINE_WIDTH", sep)
  linechart_default_line_color <- learn_line(lines, "DEFAULT_LINE_COLOR", sep)
  linechart_dots_display <- learn_line(lines, "SHOW_DOTS", sep)
  linechart_dot_size <- learn_line(lines, "DOT_SIZE", sep)
  linechart_title_display <- learn_line(lines, "SHOW_TITLE", sep)
  linechart_title_size <- learn_line(lines, "TITLE_SIZE", sep)
  linechart_title_color <- learn_line(lines, "TITLE_COLOR", sep)
  linechart_title_shift_x <- learn_line(lines, "TITLE_SHIFT_X", sep)
  linechart_title_shift_y <- learn_line(lines, "TITLE_SHIFT_Y", sep)
  linechart <- list(
    basic = list(
      colors = linechart_colors,
      x = linechart_x,
      y = linechart_y
    ),
    position = list(
      vertical = linechart_vertical,
      shift = linechart_shift
    ),
    line = list(
      display = linechart_line_display,
      width = linechart_line_width,
      default_color = linechart_default_line_color
    ),
    dot = list(
      display = linechart_dots_display,
      size = linechart_dot_size
    ),
    title = list(
      display = linechart_title_display,
      size = linechart_title_size,
      color = linechart_title_color,
      shift_x = linechart_title_shift_x,
      shift_y = linechart_title_shift_y
    )
  )
  if (is.null(unlist(linechart))) {
    linechart <- NULL
  }
  return(linechart)
}

#' Learn alignment
#'
#' @description learn alignment paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_alignment <- function(lines, sep) {
  alignment_custom_color_scheme <- learn_line(lines, "CUSTOM_COLOR_SCHEME", sep)
  alignment_color_scheme <- learn_line(lines, "COLOR_SCHEME", sep)
  alignment_highlight_references <- learn_line(lines, "HIGHLIGHT_REFERENCES", sep)
  alignment_make_references <- learn_line(lines, "MARK_REFERENCES", sep)
  alignment_reference_box_border_width <- learn_line(lines, "REFERENCE_BOX_BORDER_WIDTH", sep)
  alignment_reference_box_border_color <- learn_line(lines, "REFERENCE_BOX_BORDER_COLOR", sep)
  alignment_reference_box_fill_color <- learn_line(lines, "REFERENCE_BOX_FILL_COLOR", sep)
  alignment_start_position <- learn_line(lines, "START_POSITION", sep)
  alignment_end_position <- learn_line(lines, "END_POSITION", sep)
  alignment_highlight_type <- learn_line(lines, "HIGHLIGHT_TYPE", sep)
  alignment_highlight_disagreements <- learn_line(lines, "HIGHLIGHT_DISAGREEMENTS", sep)
  alignment_colored_dots <- learn_line(lines, "COLORED_DOTS", sep)
  alignment_inverse_gaps <- learn_line(lines, "INVERSE_GAPS", sep)
  alignment_ignore_gaps <- learn_line(lines, "IGNORE_GAPS", sep)
  alignment_consensus_display <- learn_line(lines, "DISPLAY_CONSENSUS", sep)
  alignment_consensus_threshold <- learn_line(lines, "CONSENSUS_THRESHOLD", sep)
  alignment_conservation_display <- learn_line(lines, "DISPLAY_CONSERVATION", sep)
  alignment_color_graph <- learn_line(lines, "COLOR_GRAPH", sep)
  alignment <- list(
    scheme = list(
      custom = alignment_custom_color_scheme,
      use = alignment_color_scheme
    ),
    reference = list(
      ids = alignment_highlight_references,
      use = alignment_make_references,
      box_border_width = alignment_reference_box_border_width,
      box_border_color = alignment_reference_box_border_color,
      box_fill_color = alignment_reference_box_fill_color
    ),
    position = list(
      start = alignment_start_position,
      end = alignment_end_position
    ),
    highlight = list(
      type = alignment_highlight_type,
      disagreements = alignment_highlight_disagreements,
      colored_dots = alignment_colored_dots
    ),
    gap = list(
      inverse = alignment_inverse_gaps,
      ignore = alignment_ignore_gaps
    ),
    consensus = list(
      display = alignment_consensus_display,
      threshold = alignment_consensus_threshold,
      conservation = alignment_conservation_display,
      color_graph = alignment_color_graph
    )
  )
  if (is.null(unlist(alignment))) {
    alignment <- NULL
  }
  return(alignment)
}

#' Learn connection
#'
#' @description learn connection paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_connection <- function(lines, sep) {
  draw_arrows <- learn_line(lines, "DRAW_ARROWS", sep)
  arrow_size <- learn_line(lines, "ARROW_SIZE", sep)
  loop_size <- learn_line(lines, "LOOP_SIZE", sep)
  line_width_max <- learn_line(lines, "MAXIMUM_LINE_WIDTH", sep)
  curve_angle <- learn_line(lines, "CURVE_ANGLE", sep)
  center_curves <- learn_line(lines, "CENTER_CURVES", sep)
  connection <- list(
    arrow = list(
      display = draw_arrows,
      size = arrow_size
    ),
    line = list(
      loop_size = loop_size,
      width_max = line_width_max,
      curve_angle = curve_angle,
      center_curves = center_curves
    )
  )
  if (is.null(unlist(connection))) {
    connection <- NULL
  }
  return(connection)
}

#' Learn image
#'
#' @description learn connection paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_image <- function(lines, sep) {
  image_rotation <- learn_line(lines, "IMAGE_ROTATION", sep)
  image_shift_v <- learn_line(lines, "IMAGE_SHIFT_V", sep)
  image_shift_h <- learn_line(lines, "IMAGE_SHIFT_H", sep)
  image <- list(
    rotation = image_rotation,
    shift_v = image_shift_v,
    shift_h = image_shift_h
  )
  if (is.null(unlist(image))) {
    image <- NULL
  }
  return(image)
}

#' Learn externalshape
#'
#' @description learn connection paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_externalshape <- function(lines, sep) {
  externalshape_horizontal_grid <- learn_line(lines, "HORIZONTAL_GRID", sep)
  externalshape_vertical_grid <- learn_line(lines, "VERTICAL_GRID", sep)
  externalshape_spacing <- learn_line(lines, "SHAPE_SPACING", sep)
  externalshape_type <- learn_line(lines, "SHAPE_TYPE", sep)
  externalshape_fill <- learn_line(lines, "COLOR_FILL", sep)
  externalshape <- list(
    horizontal_grid = externalshape_horizontal_grid,
    vertical_grid = externalshape_vertical_grid,
    spacing = externalshape_spacing,
    type = externalshape_type,
    fill = externalshape_fill
  )
  if (is.null(unlist(externalshape))) {
    externalshape <- NULL
  }
  return(externalshape)
}

#' Learn align
#'
#' @description learn connection paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_align <- function(lines, sep) {
  align_label <- learn_line(lines, "ALIGN_TO_LABELS", sep)
  align_field <- learn_line(lines, "ALIGN_FIELDS", sep)
  align_tree <- learn_line(lines, "ALIGN_TO_TREE", sep)
  align <- list(
    label = align_label,
    field = align_field,
    tree = align_tree
  )
  if (is.null(unlist(align))) {
    align <- NULL
  }
  return(align)
}

#' Learn binary
#'
#' @description learn binary paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_binary <- function(lines, sep) {
  binary_symbol_spacing <- learn_line(lines, "SYMBOL_SPACING", sep)
  binary <- list(spacing = binary_symbol_spacing)
  if (is.null(unlist(binary))) {
    binary <- NULL
  }
  return(binary)
}

#' Learn piechart
#'
#' @description learn piechart paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_piechart <- function(lines, sep) {
  piechart_polar_area_diagram <- learn_line(lines, "POLAR_AREA_DIAGRAM", sep)
  piechart <- list(polar_area_diagram = piechart_polar_area_diagram)
  if (is.null(unlist(piechart))) {
    piechart <- NULL
  }
  return(piechart)
}

#' Learn basic plot
#'
#' @description learn basic plot paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_basic_plot <- function(lines, sep) {
  basic_plot_dataset_scale <- learn_line(lines, "DATASET_SCALE", sep)
  basic_plot_width <- learn_line(lines, "WIDTH", sep)
  basic_plot_height_factor <- learn_line(lines, "HEIGHT_FACTOR", sep)
  basic_plot_size_max <- learn_line(lines, "MAXIMUM_SIZE", sep)
  basic_plot_value_display <- learn_line(lines, "SHOW_VALUES", sep)
  basic_plot_dashed_lines <- learn_line(lines, "DASHED_LINES", sep)
  basic_plot <- list(
    dataset_scale = basic_plot_dataset_scale,
    width = basic_plot_width,
    height_factor = basic_plot_height_factor,
    size_max = basic_plot_size_max,
    value_display = basic_plot_value_display,
    dashed_lines = basic_plot_dashed_lines
  )
  if (is.null(unlist(basic_plot))) {
    basic_plot <- NULL
  }
  return(basic_plot)
}

#' Learn basic theme
#'
#' @description learn basic theme paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_basic_theme <- function(lines, sep) {
  basic_theme_internal_display <- learn_line(lines, "SHOW_INTERNAL", sep)
  basic_theme_margin <- learn_line(lines, "MARGIN", sep)
  basic_theme_size_factor <- learn_line(lines, "SIZE_FACTOR", sep)
  basic_theme <- list(
    internal_display = basic_theme_internal_display,
    margin = basic_theme_margin,
    size_factor = basic_theme_size_factor
  )
  if (is.null(unlist(basic_theme))) {
    basic_theme <- NULL
  }
  return(basic_theme)
}

#' Learn specific themes
#'
#' @description learn specific theme paramters as list
#' @param lines file lines
#' @param sep separator
#' @param type template type
#' @export
learn_theme_specific_themes <- function(lines, sep, type) {
  if (type == "DATASET_BINARY") {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      binary = learn_theme_binary(lines, sep)
    )
  }
  if (type %in% c("DATASET_COLORSTRIP", "DATASET_GRADIENT", "DATASET_HEATMAP")) {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      strip_label = learn_theme_strip_label(lines, sep),
      heatmap = learn_theme_heatmap(lines, sep)
    )
  }
  if (type == "DATASET_EXTERNALSHAPE") {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      externalshape = learn_theme_externalshape(lines, sep)
    )
  }
  if (type %in% c("DATASET_SYMBOL", "DATASET_SIMPLEBAR", "DATASET_MULTIBAR", "DATASET_DOMAINS", "DATASET_BOXPLOT")) {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      bar = learn_theme_bar(lines, sep),
      domain = learn_theme_domain(lines, sep)
    )
  }
  if (type == "DATASET_LINECHART") {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      linechart = learn_theme_linechart(lines, sep)
    )
  }
  if (type == "DATASET_PIECHART") {
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      piechart = learn_theme_piechart(lines, sep)
    )
  }
  if (type == "DATASET_ALIGNMENT") {
    specific_themes <- list(alignment = learn_theme_alignment(lines, sep))
  }
  if (type == "DATASET_CONNECTION") { # align
    specific_themes <- list(
      basic_plot = learn_theme_basic_plot(lines, sep),
      connection = learn_theme_connection(lines, sep)
    )
  }
  if (type == "DATASET_IMAGE") {
    specific_themes <- list(image = learn_theme_image(lines, sep))
  }
  return(specific_themes)
}

#' Learn common themes
#'
#' @description learn common theme paramters as list
#' @param lines file lines
#' @param sep separator
#' @export
learn_theme_common_themes <- function(lines, sep) {
  common_themes <- list(
    legend = learn_legend(lines, sep),
    basic_theme = learn_theme_basic_theme(lines, sep),
    label = learn_theme_label(lines, sep),
    border = learn_theme_border(lines, sep),
    align = learn_theme_align(lines, sep)
  )
  return(common_themes)
}
###################################################
##                                               ##
##                   OPRATION                    ##
##                                               ##
###################################################



#' Split lines into two parts
#'
#' @description Split lines based on the data block marker
#' @param lines string vector
#' @param param "theme" or "data" for the theme paramters or the data lines
#' @export
line_split <- function(lines, param = "data") {
  if (param == "data") {
    lines_sub <- lines[-c(1:grep("^DATA$", lines))]
  } else {
    lines_sub <- lines[c(1:(grep("^DATA$", lines) - 1))]
  }
  return(lines_sub)
}

#' Learn sub data frame
#'
#' @description Learn sub data frame from template file
#' @param lines line vector
#' @param type template type
#' @param sep separator
#' @param dataset_name label in template file
#' @param field_labels sample ids for binary, heatmap, and other multi-column value templates
#' @importFrom data.table fread
#' @importFrom data.table melt
#' @importFrom tidyr separate
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_remove
#' @importFrom Biostrings readBStringSet
#' @import dplyr
#' @export
learn_subdf <- function(lines, type, sep, dataset_name = NULL, field_labels = NULL) {
  if (is.null(dataset_name)) {
    dataset_name <- type
  }
  dataset_name <- sub(" ", "_", dataset_name)
  field_labels <- sub(" ", "_", field_labels)
  colnames <- list(
    "COLLAPSE" = c("NODE_ID", "COLLAPSE"),
    "PRUNE" = c("NODE_ID", "COLLAPSE"),
    "SPACING" = c("NODE_ID", "VERTICAL_SPACING_FACTOR"),
    "TREE_COLORS" = c("NODE_ID", "TYPE", "COLOR", "LABEL_OR_STYLE", "SIZE_FACTOR"),
    "DATASET_STYLE" = c("NODE_ID", "TYPE", "WHAT", "COLOR", "WIDTH_OR_SIZE_FACTOR", "STYLE", "BACKGROUND_COLOR"),
    "LABELS" = c("NODE_ID", "LABEL"),
    "DATASET_TEXT" = c("NODE_ID", "LABEL", "POSITION", "COLOR", "STYLE", "SIZE_FACTOR", "ROTATION"),
    "DATASET_COLORSTRIP" = c("NODE_ID", "COLOR", "LABEL"),
    "DATASET_BINARY" = c("NODE_ID", field_labels),
    "DATASET_GRADIENT" = c("NODE_ID", field_labels),
    "DATASET_HEATMAP" = c("NODE_ID", field_labels),
    "DATASET_SYMBOL" = c("NODE_ID", "SYMBOL", "SIZE", "COLOR", "FILL", "POSITION"),
    "DATASET_EXTERNALSHAPE" = c("NODE_ID", field_labels),
    "DATASET_DOMAINS" = c("NODE_ID", "LENGTH", "SHAPE", "START", "END", "COLOR", "LABEL"),
    "DATASET_SIMPLEBAR" = c("NODE_ID", field_labels),
    "DATASET_MULTIBAR" = c("NODE_ID", field_labels),
    "DATASET_BOXPLOT" = c("NODE_ID", "MINIMUM", "Q1", "MEDIAN", "Q3", "MAXIMUM", "EXTREME_VALUES"),
    "DATASET_LINECHART" = c("NODE_ID", "X", "Y"),
    "DATASET_PIECHART" = c("NODE_ID", "POSITION", "RADIUS", field_labels),
    "DATASET_ALIGNMENT" = c("NODE_ID", "SEQUENCE"),
    "DATASET_CONNECTION" = c("NODE_ID", "TARGET", "WIDTH", "COLOR", "STYLE", "LABEL"),
    "DATASET_IMAGE" = c("NODE_ID", "POSITION", "SIZE_FACTOR", "ROTATION", "HORIZONTAL_SHIFT", "VERTICAL_SHIFT", "IMAGE_URL"),
    "POPUP_INFO" = c("NODE_ID", "POPUP_TITLE", "POPUP_CONTENT")
  )
  subdf_colnames <- paste0(dataset_name, "$", colnames[[type]], collapse = sep)
  if (type == "DATASET_DOMAINS") {
    df_data_tmp <- data.table::fread(text = c(paste0(dataset_name, "$", c("NODE_ID", "LENGTH", "DOMAINS"), collapse = sep), lines), sep = sep, fill = TRUE, header = TRUE)
    df_data_tmp_long <- data.table::melt(df_data_tmp, id.vars = paste0(dataset_name, "$", c("NODE_ID", "LENGTH")))
    suppressWarnings(df_data_tmp_long_separated <- df_data_tmp_long %>% tidyr::separate(value, sep = "\\|", into = paste0(dataset_name, "$", c("SHAPE", "START", "END", "COLOR", "LABEL")), remove = FALSE))
    names <- names(df_data_tmp_long_separated)
    vars <- paste0("V", c(1:(length(names) - 1)))
    names(df_data_tmp_long_separated) <- c("id", vars)
    df_data_tmp_long_separated <- df_data_tmp_long_separated %>%
      arrange(id, as.numeric(V5)) %>%
      filter(V3 != "")
    names(df_data_tmp_long_separated) <- names
    df_data <- df_data_tmp_long_separated %>% select(-variable, -value)
  } else {
    if (type == "DATASET_BOXPLOT") {
      df_data_tmp <- data.table::fread(text = c(paste0(dataset_name, "$", c("NODE_ID", "MINIMUM", "Q1", "MEDIAN", "Q3", "MAXIMUM"), collapse = sep), lines), sep = sep, fill = TRUE, header = TRUE)
      names <- names(df_data_tmp)
      names <- names[-c(1:6)]
      df_data_tmp$EXTREME_VALUES <- apply(as.data.frame(df_data_tmp)[, names], 1, paste, collapse = ",")
      df_data_tmp$EXTREME_VALUES <- stringr::str_remove_all(df_data_tmp$EXTREME_VALUES, "NA")
      df_data_tmp$EXTREME_VALUES <- stringr::str_remove(df_data_tmp$EXTREME_VALUES, ",*$")
      df_data <- df_data_tmp %>% select(-starts_with("V"))
      names(df_data)[7] <- paste0(dataset_name, "$EXTREME_VALUES")
    } else {
      if (type == "DATASET_LINECHART") {
        df_data_tmp <- data.table::fread(text = c(paste0(dataset_name, "$", c("NODE_ID", "XY"), collapse = sep), lines), sep = sep, fill = TRUE, header = TRUE)
        suppressWarnings(df_data_tmp_long <- data.table::melt(df_data_tmp, id.vars = paste0(dataset_name, "$", c("NODE_ID"))))
        suppressWarnings(df_data_tmp_long_separated <- df_data_tmp_long %>% tidyr::separate(value, sep = "\\|", into = paste0(dataset_name, "$", c("X", "Y")), remove = FALSE))
        names <- names(df_data_tmp_long_separated)
        vars <- paste0("V", c(1:(length(names) - 1)))
        names(df_data_tmp_long_separated) <- c("id", vars)
        df_data_tmp_long_separated <- df_data_tmp_long_separated %>%
          arrange(id, as.numeric(V3)) %>%
          filter(V2 != "")
        names(df_data_tmp_long_separated) <- names
        df_data <- df_data_tmp_long_separated %>% select(-variable, -value)
      } else {
        if (type == "DATASET_ALIGNMENT") {
          temp <- tempfile()
          cat(lines, file = temp, sep = "\n")
          fasta_file <- Biostrings::readBStringSet(temp)
          NODE_ID <- names(fasta_file)
          SEQUENCE <- paste(fasta_file)
          df_data <- data.frame(NODE_ID, SEQUENCE)
          df_data$NODE_ID <- as.character(df_data$NODE_ID)
          names(df_data)[2] <- paste0(dataset_name, "$SEQUENCE")
        } else {
          df_data <- data.table::fread(text = c(subdf_colnames, lines), sep = sep, fill = TRUE, header = TRUE)
        }
      }
    }
  }
  names(df_data)[1] <- "id"
  df_data$id <- as.character(df_data$id)
  return(df_data)
}

#' Merge two data frame
#'
#' @description merge sub data frame into initial data frame
#' @param df1 initial data frame
#' @param df2 sub data frame
#' @param by key column
#' @importFrom dplyr left_join
#' @export
df_merge <- function(df1, df2, by = "id") {
  df <- left_join(df1, df2, by = by)
  return(df)
}

#' Convert range to node id
#'
#' @description Convert the data frame with range id to node id by mrca method.
#' @param df data frame with any type of id
#' @param tree tree file path
#' @importFrom data.table fread
#' @importFrom ape mrca
#' @importFrom ape drop.tip
#' @importFrom methods is
#' @import dplyr
#' @export
convert_range_to_node <- function(df, tree) {
  if (length(grep("COLLAPSE|PRUNE", names(df)[2])) > 0) {
    df[, 2] <- 1
  }
  if (length(grep("\\|", df$id)) > 0) {
    text <- df$id[grep("\\|", df$id)]
    if (length(text) == 1) {
      text <- paste0(text, "\n")
    }
    df_tip1_tip2 <- data.table::fread(text = text, sep = "|", col.names = c("tip1", "tip2"), header = FALSE, )
    if (is.character(tree)) {
      tree_object <- read.tree(tree)
    }
    if (is(tree, "phylo")) {
      tree_object <- tree
    }
    mrca <- ape::mrca(tree_object)
    tips <- list()
    node_label <- c()
    df_tip1_tip2 <- as.data.frame(df_tip1_tip2)
    na_rows <- as.character(df_tip1_tip2[, 1]) %in% tree_object$tip.label | as.character(df_tip1_tip2[, 2]) %in% tree_object$tip.label
    N_tip_pairs <- nrow(df_tip1_tip2)
    for (i in 1:N_tip_pairs) {
      if (!na_rows[i]) {
        tips[[i]] <- NA
        node_label[i] <- NA
      } else {
        if (as.character(df_tip1_tip2[i, 1]) == as.character(df_tip1_tip2[i, 2])) {
          tree_object_2 <- drop.tip(tree_object, tip = as.character(df_tip1_tip2[i, 1]))
          tips[[i]] <- as.character(df_tip1_tip2[i, 1])
          node_label[i] <- setdiff(tree_object$node.label, tree_object_2$node.label)
        } else {
          node_id <- mrca[as.character(df_tip1_tip2[i, 1]), as.character(df_tip1_tip2[i, 2])]
          tips[[i]] <- unique(row.names(mrca[which(mrca == node_id, arr.ind = TRUE)[, 1], ]))
          node_label[i] <- tree_object$node.label[node_id - tree_object$Nnode - 1]
        }
      }
    }
    df$id[grep("\\|", df$id)] <- node_label
  }
  return(df)
}

#' Learn data from template file
#'
#' @description Learn data from template file into data frame
#' @param df1 initial data frame
#' @param file template file
#' @param tree tree file
#' @param ... Further arguments to be passed to subsequent functions.
#' @export
learn_data <- function(df1 = NULL, file, tree = NULL, ...) {
  file_name <- file_get_name(str = file, with_ext = FALSE, keep_dir = FALSE)
  if (is.null(df1)) {
    df1 <- learn_df(tree = tree, node = TRUE, tip = TRUE)
  }
  lines_clean <- line_clean(file = file)
  type <- learn_type(file = file)
  separator <- learn_separator(lines_clean)
  lines_data <- line_split(lines_clean)
  if (type %in% c("COLLAPSE", "PRUNE")) {
    df_data <- learn_subdf(lines = lines_data, type = type, sep = "\t", dataset_name = file_name, ...)
  }
  if (type %in% c("SPACING", "TREE_COLORS", "LABELS", "POPUP_INFO")) {
    df_data <- learn_subdf(lines = lines_data, type = type, sep = separator, dataset_name = file_name, ...)
  }
  if (type %in% c("DATASET_STYLE", "DATASET_TEXT")) {
    profile <- learn_profile(lines_clean, separator)
    common_themes <- learn_theme_common_themes(lines_clean, separator)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = separator, dataset_name = profile[["name"]], ...)
  }
  if (type %in% c("DATASET_COLORSTRIP", "DATASET_SYMBOL", "DATASET_DOMAINS", "DATASET_BOXPLOT", "DATASET_LINECHART", "DATASET_ALIGNMENT", "DATASET_CONNECTION", "DATASET_IMAGE")) {
    profile <- learn_profile(lines_clean, separator)
    common_themes <- learn_theme_common_themes(lines_clean, separator)
    specific_themes <- learn_theme_specific_themes(lines_clean, separator, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = separator, dataset_name = profile[["name"]], ...)
  }
  if (type %in% c("DATASET_GRADIENT", "DATASET_SIMPLEBAR")) {
    profile <- learn_profile(lines_clean, separator)
    common_themes <- learn_theme_common_themes(lines_clean, separator)
    specific_themes <- learn_theme_specific_themes(lines_clean, separator, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = separator, dataset_name = profile[["name"]], field_labels = profile[["name"]], ...)
  }
  if (type %in% c("DATASET_BINARY", "DATASET_HEATMAP", "DATASET_EXTERNALSHAPE", "DATASET_MULTIBAR", "DATASET_PIECHART")) {
    profile <- learn_profile(lines_clean, separator)
    field <- learn_field(lines_clean, separator)
    common_themes <- learn_theme_common_themes(lines_clean, separator)
    specific_themes <- learn_theme_specific_themes(lines_clean, separator, type)
    df_data <- learn_subdf(lines = lines_data, type = type, sep = separator, dataset_name = profile[["name"]], field_labels = field[["labels"]], ...)
  }
  df_data <- convert_range_to_node(df_data, tree)
  df1[["node"]] <- df_merge(df1[["node"]], df_data)
  df1[["tip"]] <- df_merge(df1[["tip"]], df_data)
  return(df1)
}

## Learn all data from list of units
### tree
#### samples from heatmap

#' Learn object data from unit
#'
#' @description Learn itol.hub object data from unit object.
#' @param object itol.hub object
#' @param unit itol.unit object
#' @import dplyr
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @export
learn_data_from_unit <- function(object, unit) {
  unit_name <- unique(stringr::str_remove(c(names(unit@data$node), names(unit@data$tip))[grep("\\$", c(names(unit@data$node), names(unit@data$tip)))], "\\$.*$"))
  unit_name_old <- unit_name
  if (unit_name %in% unique(stringr::str_remove(c(names(object@meta.data$node)[-1], names(object@meta.data$tip)[-1]), "\\$.*$"))) {
    count <- sum(stringr::str_count(stringr::str_remove(names(object@theme), "#.*$"), unit_name))
    unit_name <- paste0(unit_name, "#", count)
    names(unit@data$node) <- stringr::str_replace(names(unit@data$node), unit_name_old, unit_name)
    names(unit@data$tip) <- stringr::str_replace(names(unit@data$tip), unit_name_old, unit_name)
  }
  ## COLLAPSE, PRUNE, SPACING
  if (unit@type %in% c("COLLAPSE", "PRUNE", "SPACING", "DATASET_TEXT", "DATASET_COLORSTRIP", "DATASET_BINARY", "DATASET_GRADIENT", "DATASET_EXTERNALSHAPE", "DATASET_SIMPLEBAR", "DATASET_MULTIBAR", "DATASET_BOXPLOT", "DATASET_PIECHART", "DATASET_IMAGE", "POPUP_INFO")) {
    ## meta.data
    if (!is.null(unit@data)) {
      object@meta.data$node <- left_join(object@meta.data$node, unit@data$node, by = "id")
      object@meta.data$tip <- left_join(object@meta.data$tip, unit@data$tip, by = "id")
    }
  }
  ## TREE_COLORS,DATASET_STYLE
  if (unit@type %in% c("TREE_COLORS", "DATASET_STYLE", "DATASET_SYMBOL", "DATASET_LINECHART", "DATASET_CONNECTION")) {
    ## meta.data
    if (!is.null(unit@data)) {
      data_node <- unite_rows(unit@data$node)
      data_tip <- unite_rows(unit@data$tip)
      object@meta.data$node <- left_join(object@meta.data$node, data_node, by = "id")
      object@meta.data$tip <- left_join(object@meta.data$tip, data_tip, by = "id")
    }
  }
  ## LABELS
  if (unit@type == "LABELS") {
    ## taxonomy
    if (unit_name %in% unique(stringr::str_remove(c(names(object@taxonomy$node)[-1],names(object@taxonomy$tip)[-1]), "\\$.*$"))) {
      count <- sum(stringr::str_count(stringr::str_remove(names(object@theme), "#.*$"), unit_name))
      unit_name <- paste0(unit_name, "#", count)
      names(unit@data$node) <- stringr::str_replace(names(unit@data$node), unit_name_old, unit_name)
      names(unit@data$tip) <- stringr::str_replace(names(unit@data$tip), unit_name_old, unit_name)
    }
    object@taxonomy$node <- left_join(object@taxonomy$node, unit@data$node, by = "id")
    object@taxonomy$tip <- left_join(object@taxonomy$tip, unit@data$tip, by = "id")
  }
  ## DATASET_HEATMAP
  if (unit@type == "DATASET_HEATMAP") {
    unit_name_init <- unit_name
    ## filed tree
    if (!is.null(unit@specific_themes$heatmap$tree$tree)) {
      tree_text <- unit@specific_themes$heatmap$tree$tree
      object@tree$field <- append(object@tree$field, list(new_element = read.tree(text = tree_text)))
      if (unit_name %in% unique(stringr::str_remove(names(object@tree$field), "\\$.*$"))) {
        count <- sum(stringr::str_count(stringr::str_remove(names(object@theme), "#.*$"), unit_name))
        unit_name <- paste0(unit_name, "#", count)
      }
      names(object@tree$field)[length(object@tree$field)] <- unit_name
    }
    ## abundance
    unit_name <- unit_name_init
    if (!is.null(unit@data)) {
      abundance <- rbind(unit@data$node, unit@data$tip)
      names <- names(abundance)
      abundance$count_na <- rowSums(is.na(abundance))
      abundance <- filter(abundance, count_na != length(names) - 1) %>% select(-count_na)
      id <- abundance$id
      abundance <- abundance[, -1]
      abundance[abundance == "X"] <- NA
      abundance <- mutate_all(abundance, function(x) as.numeric(as.character(x)))
      abundance <- cbind(id, abundance)
      if (unit_name %in% unique(stringr::str_remove(names(object@abundance), "\\$.*$"))) {
        count <- sum(stringr::str_count(stringr::str_remove(names(object@theme), "#.*$"), unit_name))
        unit_name <- paste0(unit_name, "#", count)
        names(abundance) <- stringr::str_replace(names(abundance), unit_name_old, unit_name)
      }
      object@abundance <- left_join(object@abundance, abundance, by = "id")
    }
  }
  if (unit@type == "DATASET_DOMAINS") {
    names <- names(unit@data$node)
    vars <- paste0("V", c(1:(length(names) - 1)))
    names(unit@data$node) <- c("id", vars)
    names(unit@data$tip) <- c("id", vars)
    data_node <- unit@data$node %>%
      group_by(id, V1) %>%
      summarize(str = paste(V2, V3, V4, V5, V6, sep = "|", collapse = unit@sep))
    data_tip <- unit@data$tip %>%
      group_by(id, V1) %>%
      summarize(str = paste(V2, V3, V4, V5, V6, sep = "|", collapse = unit@sep))
    names(data_node) <- c(names[1:2], paste0(stringr::str_extract(names[3], "^.*\\$"), "DOMAINS"))
    names(data_tip) <- c(names[1:2], paste0(stringr::str_extract(names[3], "^.*\\$"), "DOMAINS"))
    object@meta.data$node <- left_join(object@meta.data$node, data_node, by = "id")
    object@meta.data$tip <- left_join(object@meta.data$tip, data_tip, by = "id")
  }
  if (unit@type == "DATASET_ALIGNMENT") {
    if (unit_name %in% unique(stringr::str_remove(names(object@seq)[-1], "\\$.*$"))) {
      count <- sum(stringr::str_count(stringr::str_remove(names(object@theme), "#.*$"), unit_name))
      unit_name <- paste0(unit_name, "#", count)
      names(object@seq) <- stringr::str_replace(names(object@seq), unit_name_old, unit_name)
    }
    object@seq <- left_join(object@seq, unit@data$tip, by = "id")
  }
  if (length(object@meta.data$node) > 1 || length(object@meta.data$tip) > 1) {
    object@meta.data$node <- object@meta.data$node[colSums(is.na(object@meta.data$node)) < nrow(object@meta.data$node)]
    object@meta.data$tip <- object@meta.data$tip[colSums(is.na(object@meta.data$tip)) < nrow(object@meta.data$tip)]
  }
  object@theme <- append(object@theme, list(new_element = create_theme(unit)))
  names(object@theme)[length(object@theme)] <- unit_name
  return(object)
}

#' Learn object data from units
#'
#' @description Learn itol.hub object data from list of unit object.
#' @param object itol.hub object
#' @param units itol.unit object list
#' @export
learn_data_from_unit_list <- function(object, units) {
  for (i in 1:length(units)) {
    object <- learn_data_from_unit(object, units[[i]])
  }
  return(object)
}

#' Learn object data from file
#'
#' @description Learn itol.hub object data from template file.
#' @param object itol.hub object
#' @param file template file
#' @export
learn_data_from_file <- function(object, file) {
  unit <- file_to_unit(file = file, tree = object@tree$main)
  object <- learn_data_from_unit(object, unit)
  return(object)
}

#' Learn object data from files
#'
#' @description Learn itol.hub object data from template file.
#' @param object itol.hub object
#' @param files template files path
#' @param dir files path
#' @param pattern file name pattern in regex
#' @param ... Further arguments to be passed to subsequent functions.
#' @export
learn_data_from_files <- function(object, files = NULL, dir = NULL, pattern = ".", ...) {
  if (!is.null(dir)) {
    files <- dir(path = dir, pattern = pattern, full.names = TRUE, ...)
  }
  for (i in 1:length(files)) {
    unit <- file_to_unit(file = files[i], tree = object@tree$main)
    object <- learn_data_from_unit(object, unit)
  }
  return(object)
}

## Network

#' Convert character data to 0/1
#'
#' @description In data frame and list, convert character and numberic data to 0/1.
#' @param object data frame or list
#' @importFrom dplyr mutate_all
#' @export
convert_01 <- function(object) {
  elements_number <- length(object)
  for (i in 1:elements_number) {
    object[[i]][object[[i]] != "" & !is.na(object[[i]])] <- 1
    object[[i]][object[[i]] == "" | is.na(object[[i]])] <- 0
  }
  object <- dplyr::mutate_all(object, function(x) as.numeric(as.character(x)))
  return(object)
}

#' Convert 0/1 data to connection pairs
#'
#' @description If two column has more than 1 shared element then they have connection. Convert
#' 0/1 data to connection pairs in long shape table. The 0-connection pairs are removed.
#' @param object data frame with 0/1 data
#' @export
convert_01_to_connect <- function(object) {
  elements_number <- length(object)
  m <- matrix(NA, nrow = elements_number, ncol = elements_number)
  for (i in 1:elements_number) {
    for (j in 1:elements_number) {
      m[i, j] <- sum((object[[i]] + object[[j]]) == 2)
    }
  }
  n <- names(object)
  rownames(m) <- n
  colnames(m) <- n
  nn <- dimnames(m)
  ind <- which(upper.tri(m, diag = TRUE), arr.ind = TRUE)
  df <- data.frame(
    row = nn[[1]][ind[, 1]],
    col = nn[[2]][ind[, 2]],
    val = m[ind]
  )
  return(df)
}


utils::globalVariables(c("NODE_ID", "START", "value", "variable", "X"))
