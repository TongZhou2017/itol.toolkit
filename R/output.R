#' Write unit object into file
#'
#' @description Write itol.unit object into template file. This function will
#' using the type information in unit object to decide different output methods
#' for the template formats.
#' @param unit unit object. The unit object holds the data and theme of a single
#' dataset. This is the smallest data operation unit. At this level, individual
#' data can be fine-tuned. It is also possible to extract the style of a unit
#' for use in other units. It is also possible to use many units to learn a
#' complete itol.hub object. Almost all specific data operations behind the
#' itol.toolkit package are performed at the unit level. Because itol.hub
#' objects have comprehensive information, but to ensure that the correspondence
#' with phylogenetic branches or nodes remains consistent when different data
#' types are saved, many complex data aggregations are saved, which does not
#' facilitate data processing. Therefore, in the actual data processing process,
#' unit objects are generated from the itol.hub object and then processed.
#' @param file output file path. Define the output file location and file name
#' using absolute or relative path.
#' @importFrom stats na.omit
#' @importFrom stringr str_replace_all
#' @importFrom utils write.table
#' @importFrom utils file_test
#' @importFrom magrittr %>%
#' @importFrom dplyr select starts_with summarize summarise group_by
#' @return No return value, only output a template file
#' @export
#' @examples
#' tree <- system.file("extdata",
#'                     "tree_of_itol_templates.tree",
#'                     package = "itol.toolkit")
#' data("template_groups")
#' df_group <- data.frame(id = unique(template_groups$group),
#'                        data = unique(template_groups$group))
#' ## create unit
#' unit <- create_unit(data = df_group,
#'                     key = "Quickstart",
#'                     type = "DATASET_COLORSTRIP",
#'                     tree = tree)
#' ## write unit
#' write_unit(unit,tempfile())
write_unit <- function(unit, file = getwd()) {
  if(file_test("-d",file)){
    file = paste0(file,"/",unique(stringr::str_remove(c(names(unit@data$node)[-1],names(unit@data$tip)[-1]),"\\$.*$")),".txt")
  }
  lines <- paste0(unit@type)
  if (!is.null(unit@sep)) {
    sep <- case_when(
      unit@sep == "," ~ "COMMA",
      unit@sep == "\t" ~ "TAB",
      unit@sep == " " ~ "SPACE"
    )
    lines <- c(lines, paste0("SEPARATOR ", sep))
  }
  # profile
  if (!is.null(unit@profile$name)) {
    lines <- c(lines, paste("DATASET_LABEL", unit@profile$name, sep = unit@sep))
  }
  if (!is.null(unit@profile$color)) {
    lines <- c(lines, paste("COLOR", unit@profile$color, sep = unit@sep))
  }
  # field
  if (!is.null(unit@field$labels)) {
    lines <- c(lines, paste("FIELD_LABELS", paste(unit@field$labels, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@field$colors)) {
    lines <- c(lines, paste("FIELD_COLORS", paste(unit@field$colors, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@field$shapes)) {
    lines <- c(lines, paste("FIELD_SHAPES", paste(unit@field$shapes, collapse = unit@sep), sep = unit@sep))
  }
  # common_themes
  ## legend
  if (!is.null(unit@common_themes$legend$title)) {
    lines <- c(lines, paste("LEGEND_TITLE", paste(unit@common_themes$legend$title, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$position_x)) {
    lines <- c(lines, paste("LEGEND_POSITION_X", paste(unit@common_themes$legend$position_x, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$position_y)) {
    lines <- c(lines, paste("LEGEND_POSITION_Y", paste(unit@common_themes$legend$position_y, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$horizontal)) {
    lines <- c(lines, paste("LEGEND_HORIZONTAL", paste(unit@common_themes$legend$horizontal, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$shapes)) {
    lines <- c(lines, paste("LEGEND_SHAPES", paste(unit@common_themes$legend$shapes, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$colors)) {
    lines <- c(lines, paste("LEGEND_COLORS", paste(unit@common_themes$legend$colors, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$labels)) {
    lines <- c(lines, paste("LEGEND_LABELS", paste(unit@common_themes$legend$labels, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$legend$shape_scales)) {
    lines <- c(lines, paste("LEGEND_SHAPE_SCALES", paste(unit@common_themes$legend$shape_scales, collapse = unit@sep), sep = unit@sep))
  }
  ## basic_theme
  if (!is.null(unit@common_themes$basic_theme$internal_display)) {
    lines <- c(lines, paste("SHOW_INTERNAL", paste(unit@common_themes$basic_theme$internal_display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$basic_theme$margin)) {
    lines <- c(lines, paste("MARGIN", paste(unit@common_themes$basic_theme$margin, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$basic_theme$size_factor)) {
    lines <- c(lines, paste("SIZE_FACTOR", paste(unit@common_themes$basic_theme$size_factor, collapse = unit@sep), sep = unit@sep))
  }
  ## label
  if (!is.null(unit@common_themes$label$display)) {
    lines <- c(lines, paste("SHOW_LABELS", paste(unit@common_themes$label$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$size)) {
    lines <- c(lines, paste("LABEL_SIZE_FACTOR", paste(unit@common_themes$label$size, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$top)) {
    lines <- c(lines, paste("LABELS_ON_TOP", paste(unit@common_themes$label$top, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$below)) {
    lines <- c(lines, paste("LABELS_BELOW", paste(unit@common_themes$label$below, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$rotation)) {
    lines <- c(lines, paste("LABEL_ROTATION", paste(unit@common_themes$label$rotation, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$straight)) {
    lines <- c(lines, paste("STRAIGHT_LABELS", paste(unit@common_themes$label$straight, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$vertical)) {
    lines <- c(lines, paste("VERTICAL_SHIFT", paste(unit@common_themes$label$vertical, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$shift)) {
    lines <- c(lines, paste("LABEL_SHIFT", paste(unit@common_themes$label$shift, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$label$external_shift)) {
    lines <- c(lines, paste("EXTERNAL_LABEL_SHIFT", paste(unit@common_themes$label$external_shift, collapse = unit@sep), sep = unit@sep))
  }
  ## border
  if (!is.null(unit@common_themes$border$width)) {
    lines <- c(lines, paste("BORDER_WIDTH", paste(unit@common_themes$border$width, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$border$color)) {
    lines <- c(lines, paste("BORDER_COLOR", paste(unit@common_themes$border$color, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$border$complete)) {
    lines <- c(lines, paste("COMPLETE_BORDER", paste(unit@common_themes$border$complete, collapse = unit@sep), sep = unit@sep))
  }
  ## align
  if (!is.null(unit@common_themes$align$label)) {
    lines <- c(lines, paste("ALIGN_TO_LABELS", paste(unit@common_themes$align$label, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$align$field)) {
    lines <- c(lines, paste("ALIGN_FIELDS", paste(unit@common_themes$align$field, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@common_themes$align$tree)) {
    lines <- c(lines, paste("ALIGN_TO_TREE", paste(unit@common_themes$align$tree, collapse = unit@sep), sep = unit@sep))
  }
  # specific_themes
  ## basic_plot learn_theme_basic_plot
  if (!is.null(unit@specific_themes$basic_plot$dataset_scale)) {
    lines <- c(lines, paste("DATASET_SCALE", paste(unit@specific_themes$basic_plot$dataset_scale, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$basic_plot$width)) {
    lines <- c(lines, paste("WIDTH", paste(unit@specific_themes$basic_plot$width, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$basic_plot$height_factor)) {
    lines <- c(lines, paste("HEIGHT_FACTOR", paste(unit@specific_themes$basic_plot$height_factor, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$basic_plot$size_max)) {
    lines <- c(lines, paste("MAXIMUM_SIZE", paste(unit@specific_themes$basic_plot$size_max, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$basic_plot$value_display)) {
    if(unit@type == "DATASET_SIMPLEBAR"){
      lines <- c(lines, paste("SHOW_VALUE", paste(unit@specific_themes$basic_plot$value_display, collapse = unit@sep), sep = unit@sep))
    }else{
      lines <- c(lines, paste("SHOW_VALUES", paste(unit@specific_themes$basic_plot$value_display, collapse = unit@sep), sep = unit@sep))
    }
  }
  if (!is.null(unit@specific_themes$basic_plot$dashed_lines)) {
    lines <- c(lines, paste("DASHED_LINES", paste(unit@specific_themes$basic_plot$dashed_lines, collapse = unit@sep), sep = unit@sep))
  }
  ## binary learn_theme_binary
  if (!is.null(unit@specific_themes$binary$spacing)) {
    lines <- c(lines, paste("SYMBOL_SPACING", paste(unit@specific_themes$binary$spacing, collapse = unit@sep), sep = unit@sep))
  }
  ## strip_label learn_theme_strip_label
  if (!is.null(unit@specific_themes$strip_label$display)) {
    lines <- c(lines, paste("SHOW_STRIP_LABELS", paste(unit@specific_themes$strip_label$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$width)) {
    lines <- c(lines, paste("STRIP_WIDTH", paste(unit@specific_themes$strip_label$width, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$size)) {
    lines <- c(lines, paste("STRIP_LABEL_SIZE_FACTOR", paste(unit@specific_themes$strip_label$size, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$color)) {
    lines <- c(lines, paste("STRIP_LABEL_COLOR", paste(unit@specific_themes$strip_label$color, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$color_branches)) {
    lines <- c(lines, paste("COLOR_BRANCHES", paste(unit@specific_themes$strip_label$color_branches, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$position)) {
    lines <- c(lines, paste("STRIP_LABEL_POSITION", paste(unit@specific_themes$strip_label$position, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$shift)) {
    lines <- c(lines, paste("STRIP_LABEL_SHIFT", paste(unit@specific_themes$strip_label$shift, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$rotation)) {
    lines <- c(lines, paste("STRIP_LABEL_ROTATION", paste(unit@specific_themes$strip_label$rotation, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$strip_label$outline_width)) {
    lines <- c(lines, paste("STRIP_LABEL_OUTLINE", paste(unit@specific_themes$strip_label$outline_width, collapse = unit@sep), sep = unit@sep))
  }
  ## heatmap learn_theme_heatmap
  ### color
  if (!is.null(unit@specific_themes$heatmap$color$nan)) {
    lines <- c(lines, paste("COLOR_NAN", paste(unit@specific_themes$heatmap$color$nan, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$color$min)) {
    lines <- c(lines, paste("COLOR_MIN", paste(unit@specific_themes$heatmap$color$min, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$color$mid)) {
    lines <- c(lines, paste("COLOR_MID", paste(unit@specific_themes$heatmap$color$mid, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$color$max)) {
    lines <- c(lines, paste("COLOR_MAX", paste(unit@specific_themes$heatmap$color$max, collapse = unit@sep), sep = unit@sep))
  }
  ### value
  if (!is.null(unit@specific_themes$heatmap$value$min)) {
    lines <- c(lines, paste("USER_MIN_VALUE", paste(unit@specific_themes$heatmap$value$min, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$value$mid)) {
    lines <- c(lines, paste("USER_MID_VALUE", paste(unit@specific_themes$heatmap$value$mid, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$value$max)) {
    lines <- c(lines, paste("USER_MAX_VALUE", paste(unit@specific_themes$heatmap$value$max, collapse = unit@sep), sep = unit@sep))
  }
  ### tree
  if (!is.null(unit@specific_themes$heatmap$tree$tree_display)) {
    lines <- c(lines, paste("SHOW_TREE", paste(unit@specific_themes$heatmap$tree$tree_display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$heatmap$tree$tree)) {
    if(unit@specific_themes$heatmap$tree$tree_display == 1){
      lines <- c(lines, paste("FIELD_TREE", paste(unit@specific_themes$heatmap$tree$tree, collapse = unit@sep), sep = unit@sep))
    }
  }
  ### use_mid
  if (!is.null(unit@specific_themes$heatmap$use_mid)) {
    lines <- c(lines, paste("USE_MID_COLOR", paste(unit@specific_themes$heatmap$use_mid, collapse = unit@sep), sep = unit@sep))
  }
  ### auto_legend
  if (!is.null(unit@specific_themes$heatmap$auto_legend)) {
    lines <- c(lines, paste("AUTO_LEGEND", paste(unit@specific_themes$heatmap$auto_legend, collapse = unit@sep), sep = unit@sep))
  }
  ## externalshape learn_theme_externalshape
  if (!is.null(unit@specific_themes$externalshape$horizontal_grid)) {
    lines <- c(lines, paste("HORIZONTAL_GRID", paste(unit@specific_themes$externalshape$horizontal_grid, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$externalshape$vertical_grid)) {
    lines <- c(lines, paste("VERTICAL_GRID", paste(unit@specific_themes$externalshape$vertical_grid, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$externalshape$spacing)) {
    lines <- c(lines, paste("SHAPE_SPACING", paste(unit@specific_themes$externalshape$spacing, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$externalshape$type)) {
    lines <- c(lines, paste("SHAPE_TYPE", paste(unit@specific_themes$externalshape$type, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$externalshape$fill)) {
    lines <- c(lines, paste("COLOR_FILL", paste(unit@specific_themes$externalshape$fill, collapse = unit@sep), sep = unit@sep))
  }
  ## bar learn_theme_bar
  if (!is.null(unit@specific_themes$bar$shift)) {
    lines <- c(lines, paste("BAR_SHIFT", paste(unit@specific_themes$bar$shift, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$bar$zero)) {
    lines <- c(lines, paste("BAR_ZERO", paste(unit@specific_themes$bar$zero, collapse = unit@sep), sep = unit@sep))
  }
  ## domain learn_theme_domain
  ### label_display
  if (!is.null(unit@specific_themes$domain$label_display)) {
    lines <- c(lines, paste("SHOW_DOMAIN_LABELS", paste(unit@specific_themes$domain$label_display, collapse = unit@sep), sep = unit@sep))
  }
  ### gradient_fill
  if (!is.null(unit@specific_themes$domain$gradient_fill)) {
    lines <- c(lines, paste("GRADIENT_FILL", paste(unit@specific_themes$domain$gradient_fill, collapse = unit@sep), sep = unit@sep))
  }
  ### backbone
  if (!is.null(unit@specific_themes$domain$backbone$color)) {
    lines <- c(lines, paste("BACKBONE_COLOR", paste(unit@specific_themes$domain$backbone$color, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$domain$backbone$height)) {
    lines <- c(lines, paste("BACKBONE_HEIGHT", paste(unit@specific_themes$domain$backbone$height, collapse = unit@sep), sep = unit@sep))
  }
  ## linechart learn_theme_linechart
  ### basic
  if (!is.null(unit@specific_themes$linechart$basic$colors)) {
    lines <- c(lines, paste("LINE_COLORS", paste(unit@specific_themes$linechart$basic$colors, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$basic$x)) {
    lines <- c(lines, paste("AXIS_X", paste(unit@specific_themes$linechart$basic$x, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$basic$y)) {
    lines <- c(lines, paste("AXIS_Y", paste(unit@specific_themes$linechart$basic$y, collapse = unit@sep), sep = unit@sep))
  }
  ### position
  if (!is.null(unit@specific_themes$linechart$position$vertical)) {
    lines <- c(lines, paste("VERTICAL_CHART", paste(unit@specific_themes$linechart$position$vertical, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$position$shift)) {
    lines <- c(lines, paste("CHART_SHIFT", paste(unit@specific_themes$linechart$position$shift, collapse = unit@sep), sep = unit@sep))
  }
  ### line
  if (!is.null(unit@specific_themes$linechart$line$display)) {
    lines <- c(lines, paste("SHOW_LINE", paste(unit@specific_themes$linechart$line$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$line$width)) {
    lines <- c(lines, paste("LINE_WIDTH", paste(unit@specific_themes$linechart$line$width, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$line$default_color)) {
    lines <- c(lines, paste("DEFAULT_LINE_COLOR", paste(unit@specific_themes$linechart$line$default_color, collapse = unit@sep), sep = unit@sep))
  }
  ### dot
  if (!is.null(unit@specific_themes$linechart$dot$display)) {
    lines <- c(lines, paste("SHOW_DOTS", paste(unit@specific_themes$linechart$dot$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$dot$size)) {
    lines <- c(lines, paste("DOT_SIZE", paste(unit@specific_themes$linechart$dot$size, collapse = unit@sep), sep = unit@sep))
  }
  ### title
  if (!is.null(unit@specific_themes$linechart$title$display)) {
    lines <- c(lines, paste("SHOW_TITLE", paste(unit@specific_themes$linechart$title$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$title$size)) {
    lines <- c(lines, paste("TITLE_SIZE", paste(unit@specific_themes$linechart$title$size, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$title$color)) {
    lines <- c(lines, paste("TITLE_COLOR", paste(unit@specific_themes$linechart$title$color, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$title$shift_x)) {
    lines <- c(lines, paste("TITLE_SHIFT_X", paste(unit@specific_themes$linechart$title$shift_x, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$linechart$title$shift_y)) {
    lines <- c(lines, paste("TITLE_SHIFT_Y", paste(unit@specific_themes$linechart$title$shift_y, collapse = unit@sep), sep = unit@sep))
  }
  ## piechart learn_theme_piechart
  if (!is.null(unit@specific_themes$piechart$polar_area_diagram)) {
    lines <- c(lines, paste("POLAR_AREA_DIAGRAM", paste(unit@specific_themes$piechart$polar_area_diagram, collapse = unit@sep), sep = unit@sep))
  }
  ## alignment learn_theme_alignment
  ### scheme
  if (!is.null(unit@specific_themes$alignment$scheme$custom)) {
    lines <- c(lines, paste("CUSTOM_COLOR_SCHEME", paste(unit@specific_themes$alignment$scheme$custom, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$scheme$use)) {
    lines <- c(lines, paste("COLOR_SCHEME", paste(unit@specific_themes$alignment$scheme$use, collapse = unit@sep), sep = unit@sep))
  }
  ### reference
  if (!is.null(unit@specific_themes$alignment$reference$ids)) {
    lines <- c(lines, paste("HIGHLIGHT_REFERENCES", paste(unit@specific_themes$alignment$reference$ids, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$reference$use)) {
    lines <- c(lines, paste("MARK_REFERENCES", paste(unit@specific_themes$alignment$reference$use, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$reference$box_border_width)) {
    lines <- c(lines, paste("REFERENCE_BOX_BORDER_WIDTH", paste(unit@specific_themes$alignment$reference$box_border_width, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$reference$box_border_color)) {
    lines <- c(lines, paste("REFERENCE_BOX_BORDER_COLOR", paste(unit@specific_themes$alignment$reference$box_border_color, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$reference$box_fill_color)) {
    lines <- c(lines, paste("REFERENCE_BOX_FILL_COLOR", paste(unit@specific_themes$alignment$reference$box_fill_color, collapse = unit@sep), sep = unit@sep))
  }
  ### position
  if (!is.null(unit@specific_themes$alignment$position$start)) {
    lines <- c(lines, paste("START_POSITION", paste(unit@specific_themes$alignment$position$start, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$position$end)) {
    lines <- c(lines, paste("END_POSITION", paste(unit@specific_themes$alignment$position$end, collapse = unit@sep), sep = unit@sep))
  }
  ### highlight
  if (!is.null(unit@specific_themes$alignment$highlight$type)) {
    lines <- c(lines, paste("HIGHLIGHT_TYPE", paste(unit@specific_themes$alignment$highlight$type, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$highlight$disagreements)) {
    lines <- c(lines, paste("HIGHLIGHT_DISAGREEMENTS", paste(unit@specific_themes$alignment$highlight$disagreements, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$highlight$colored_dots)) {
    lines <- c(lines, paste("COLORED_DOTS", paste(unit@specific_themes$alignment$highlight$colored_dots, collapse = unit@sep), sep = unit@sep))
  }
  ### gap
  if (!is.null(unit@specific_themes$alignment$gap$inverse)) {
    lines <- c(lines, paste("INVERSE_GAPS", paste(unit@specific_themes$alignment$gap$inverse, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$gap$ignore)) {
    lines <- c(lines, paste("IGNORE_GAPS", paste(unit@specific_themes$alignment$gap$ignore, collapse = unit@sep), sep = unit@sep))
  }
  ### consensus
  if (!is.null(unit@specific_themes$alignment$consensus$display)) {
    lines <- c(lines, paste("DISPLAY_CONSENSUS", paste(unit@specific_themes$alignment$consensus$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$consensus$threshold)) {
    lines <- c(lines, paste("CONSENSUS_THRESHOLD", paste(unit@specific_themes$alignment$consensus$threshold, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$consensus$conservation)) {
    lines <- c(lines, paste("DISPLAY_CONSERVATION", paste(unit@specific_themes$alignment$consensus$conservation, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$alignment$consensus$color_graph)) {
    lines <- c(lines, paste("COLOR_GRAPH", paste(unit@specific_themes$alignment$consensus$color_graph, collapse = unit@sep), sep = unit@sep))
  }
  ## connection learn_theme_connection
  ### arrow
  if (!is.null(unit@specific_themes$connection$arrow$display)) {
    lines <- c(lines, paste("DRAW_ARROWS", paste(unit@specific_themes$connection$arrow$display, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$connection$arrow$size)) {
    lines <- c(lines, paste("ARROW_SIZE", paste(unit@specific_themes$connection$arrow$size, collapse = unit@sep), sep = unit@sep))
  }
  ### line
  if (!is.null(unit@specific_themes$connection$line$loop_size)) {
    lines <- c(lines, paste("LOOP_SIZE", paste(unit@specific_themes$connection$line$loop_size, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$connection$line$width_max)) {
    lines <- c(lines, paste("MAXIMUM_LINE_WIDTH", paste(unit@specific_themes$connection$line$width_max, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$connection$line$curve_angle)) {
    lines <- c(lines, paste("CURVE_ANGLE", paste(unit@specific_themes$connection$line$curve_angle, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$connection$line$center_curves)) {
    lines <- c(lines, paste("CENTER_CURVES", paste(unit@specific_themes$connection$line$center_curves, collapse = unit@sep), sep = unit@sep))
  }
  ## image learn_theme_image
  if (!is.null(unit@specific_themes$image$rotation)) {
    lines <- c(lines, paste("IMAGE_ROTATION", paste(unit@specific_themes$image$rotation, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$image$shift_v)) {
    lines <- c(lines, paste("IMAGE_SHIFT_V", paste(unit@specific_themes$image$shift_v, collapse = unit@sep), sep = unit@sep))
  }
  if (!is.null(unit@specific_themes$image$shift_h)) {
    lines <- c(lines, paste("IMAGE_SHIFT_H", paste(unit@specific_themes$image$shift_h, collapse = unit@sep), sep = unit@sep))
  }
  # data
  lines <- c(lines, "DATA")
  ## one column: COLLAPSE, PRUNE
  simple_case <- c("SPACING", "TREE_COLORS", "LABELS", "POPUP_INFO", "DATASET_STYLE", "DATASET_TEXT", "DATASET_COLORSTRIP", "DATASET_SYMBOL", "DATASET_CONNECTION", "DATASET_IMAGE", "DATASET_GRADIENT", "DATASET_SIMPLEBAR")
  field_case <- c("DATASET_BINARY", "DATASET_HEATMAP", "DATASET_EXTERNALSHAPE", "DATASET_MULTIBAR", "DATASET_PIECHART")
  if (unit@type %in% c("COLLAPSE", "PRUNE")) {
    if (dim(unit@data$node)[2] < dim(unit@data$tip)[2] || is.null(unit@data$node)) {
      data <- unit@data$tip %>% na.omit()
    } else {
      if (dim(unit@data$node)[2] > dim(unit@data$tip)[2] || is.null(unit@data$tip)) {
        data <- unit@data$node %>% na.omit()
      } else {
        data <- rbind(unit@data$node, unit@data$tip) %>% na.omit()
      }
    }
    #data <- rbind(unit@data$node, unit@data$tip) %>% na.omit()
    lines <- c(lines, paste(data$id, sep = ""))
  } else {
    if (dim(unit@data$node)[2] < dim(unit@data$tip)[2] || is.null(unit@data$node)) {
      data <- unit@data$tip
    } else {
      if (dim(unit@data$node)[2] > dim(unit@data$tip)[2] || is.null(unit@data$tip)) {
        data <- unit@data$node
      } else {
        data <- rbind(unit@data$node, unit@data$tip)
      }
    }
    if (unit@type %in% c(simple_case, field_case)) {
      names <- names(data)
      data$count_na <- rowSums(is.na(data))
      data <- filter(data, count_na != length(names) - 1) %>% select(-count_na)
      if (unit@type %in% c("DATASET_HEATMAP")) {
        data[is.na(data)] <- "X"
      } else {
        data[is.na(data)] <- ""
      }
    }
    ## domain
    if (unit@type == "DATASET_DOMAINS") {
      names <- names(data)
      data$count_na <- rowSums(is.na(data))
      data <- filter(data, count_na != length(names) - 1) %>% select(-count_na)
      data[is.na(data)] <- ""
      vars <- paste0("V", c(1:(length(names) - 1)))
      names(data) <- c("id", vars)
      lett_orders <- unique(data$id)
      data <- data %>%
        group_by(id, V1) %>%
        summarize(str = paste(V2, V3, V4, V5, V6, sep = "|", collapse = unit@sep))
      data$id <- factor(data$id, levels = lett_orders)
    }
    ## DATASET_BOXPLOT
    if (unit@type == "DATASET_BOXPLOT") {
      names <- names(data)
      data$count_na <- rowSums(is.na(data))
      data <- filter(data, count_na != length(names) - 1) %>% select(-count_na)
      data[is.na(data)] <- ""
      data[, 7] <- stringr::str_replace_all(data[, 7], ",", unit@sep)
    }
    ## DATASET_LINECHART
    if (unit@type == "DATASET_LINECHART") {
      names <- names(data)
      data$count_na <- rowSums(is.na(data))
      data <- filter(data, count_na != length(names) - 1) %>% select(-count_na)
      data[is.na(data)] <- ""
      vars <- paste0("V", c(1:(length(names) - 1)))
      names(data) <- c("id", vars)
      lett_orders <- unique(data$id)
      data <- data %>%
        group_by(id) %>%
        summarize(str = paste(V1, V2, sep = "|", collapse = unit@sep))
      data$id <- factor(data$id, levels = lett_orders)
      names(data) <- names
    }
    ## DATASET_ALIGNMENT
    if (unit@type == "DATASET_ALIGNMENT") {
      names <- names(data)
      data$count_na <- rowSums(is.na(data))
      data <- filter(data, count_na != length(names) - 1) %>% select(-count_na)
      data[is.na(data)] <- ""
    }
  }

  writeLines(lines, con = file, sep = "\n")
  if (unit@type %in% c(simple_case, field_case, "DATASET_DOMAINS", "DATASET_BOXPLOT", "DATASET_LINECHART")) {
    write.table(data, file, col.names = FALSE, row.names = FALSE, append = TRUE, sep = unit@sep, quote = FALSE)
  }
  if (unit@type == "DATASET_ALIGNMENT") {
    fa_write(data, file, id = "id", seq = names[2], append = TRUE)
  }
}

#' Write all data object into files
#'
#' @description Write itol.hub object into template files.
#' @param object itol.hub object holds the complete data and theme information.
#' This is an all-in-one object that collects all the information. Based on this
#' object, it is possible to export template files directly. It can also be
#' converted to an operation unit object for the detailed processing of
#' individual datasets. The object can also be saved locally for reproducible
#' visualization to share. This object contains species or sample clustering
#' trees, sequence alignment, species abundance or gene expression table,
#' multi-level taxonomic information, metadata, and a list of custom themes.
#' Each element name in the theme list is prefixed with the column name of the
#' metadata and is used to establish the association between the theme and the
#' data. For some special dataset types, the storage location is not in the
#' metadata, but it also conforms to the association with themes. The program
#' automatically decides where to read the data according to the different
#' output template types. The user only needs to explicitly define the theme
#' name to be output consistent with the data name prefix.
#' @param dir output dir path. Define the output files location using absolute
#' or relative path. The template files will output by the key information from
#' theme name in the hub object.
#' @param with_tree output with tree file in newick format.
#' @return No return value, only output template files
#' @export
#' @examples
#' tree <- system.file("extdata",
#'                     "tree_of_itol_templates.tree",
#'                     package = "itol.toolkit")
#' hub <- create_hub(tree = tree)
#' data("template_groups")
#' df_group <- data.frame(id = unique(template_groups$group),
#'                        data = unique(template_groups$group))
#' ## create unit
#' unit_1 <- create_unit(data = df_group,
#'                       key = "Quickstart_1",
#'                       type = "TREE_COLORS",
#'                       subtype = "clade",
#'                       line_type = c(rep("normal",4),"dashed"),
#'                       size_factor = 5,
#'                       tree = tree)
#' unit_2 <- create_unit(data = df_group,
#'                       key = "Quickstart_2",
#'                       type = "DATASET_COLORSTRIP",
#'                       tree = tree)
#' ## write hub
#' hub <- hub + unit_1 + unit_2
#' write_hub(hub,tempdir())
write_hub <- function(object, dir = getwd(), with_tree = FALSE) {
  keys <- names(object@theme)
  for (key in keys) {
    unit <- hub_to_unit(object, object@theme[[key]], key)
    write_unit(unit, dir)
  }
  if(with_tree){
    ape::write.tree(object@tree$main, paste0(dir,"/tree.nwk"))
  }
}

#' Write raw data into files
#'
#' @description Write raw data in itol.hub object into files
#' @param object itol.hub object holds the complete data and theme information.
#' This is an all-in-one object that collects all the information. Based on this
#' object, it is possible to export template files directly. It can also be
#' converted to an operation unit object for the detailed processing of
#' individual datasets. The object can also be saved locally for reproducible
#' visualization to share. This object contains species or sample clustering
#' trees, sequence alignment, species abundance or gene expression table,
#' multi-level taxonomic information, metadata, and a list of custom themes.
#' Each element name in the theme list is prefixed with the column name of the
#' metadata and is used to establish the association between the theme and the
#' data. For some special dataset types, the storage location is not in the
#' metadata, but it also conforms to the association with themes. The program
#' automatically decides where to read the data according to the different
#' output template types. The user only needs to explicitly define the theme
#' name to be output consistent with the data name prefix.
#' @param dir output dir path. Define the output files location using absolute
#' or relative path. The raw data will write into files. The following raw data
#' will be outputted: main tree, sample tree, alignment sequences, abundance
#' count table, taxonomy table, metadata on nodes and tips.
#' @param title files name title string. This character specified the prefix of
#' raw data files.
#' @importFrom ape write.tree
#' @importFrom data.table fwrite
#' @return No return value, only output raw data files
#' @export
#' @examples
#' tree <- system.file("extdata",
#'                     "tree_of_itol_templates.tree",
#'                     package = "itol.toolkit")
#' hub <- create_hub(tree = tree)
#' df_values <- data.table::fread(system.file("extdata",
#'                                            "templates_frequence.txt",
#'                                            package = "itol.toolkit"))
#' unit <- create_unit(data = df_values,
#'                     key = "Quickstart",
#'                     type = "DATASET_HEATMAP",
#'                     tree = tree)
#' hub <- hub + unit
#' write_raw(hub,tempdir(),"Quickstart")
write_raw <- function(object, dir, title) {
  write.tree(phy = object@tree$main, file = paste0(dir,"/",title,".tree"))
  if(length(object@tree$field)>0){
    field_tree_names <- names(object@tree$field)
    for (field_tree_name in field_tree_names) {
      write.tree(phy = object@tree$field[[field_tree_name]], file = paste0(dir,"/",title,"_",field_tree_name,".tree"))
    }
  }
  if(length(names(object@seq))>1){
    seq_names <- names(object@seq)
    for (i in 1:(length(names(object@seq))-1)) {
      fa_write(object = object@seq,file = paste0(dir,"/",title,"_",seq_names[i+1],".align.fa"),id = seq_names[1],seq = seq_names[i+1])
    }
  }
  if(length(names(object@abundance))>1){
    abundance_table_titles <- unique(stringr::str_remove(names(object@abundance),"\\$.*$"))[-1]
    for (abundance_table_title in abundance_table_titles) {
      abundance_table <- object@abundance %>% select(id,starts_with(paste0("^",abundance_table_title,"\\$")))
      fwrite(x = abundance_table,file = paste0(dir,"/",title,"_",abundance_table_title,".tab.txt"))
    }
  }
  if(length(names(object@taxonomy))>1){
      fwrite(x = object@taxonomy,file = paste0(dir,"/",title,"_tax.txt"))
  }
  if(length(names(object@meta.data$node))>1){
    fwrite(x = object@meta.data$node,file = paste0(dir,"/",title,"_node_metadata.txt"))
  }
  if(length(names(object@meta.data$tip))>1){
    fwrite(x = object@meta.data$tip,file = paste0(dir,"/",title,"_tip_metadata.txt"))
  }
}

##
utils::globalVariables(c("COLOR", "END", "LABEL", "LENGTH", "SHAPE", "Y", "count_na", "V1", "V2", "V3", "V4", "V5", "V6"))
