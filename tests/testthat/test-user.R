test_that("use.theme works in default paramter", {
  expect_equal(use.theme("COLLAPSE")@type, "COLLAPSE")
  expect_equal(use.theme("PRUNE")@type, "PRUNE")
  expect_equal(use.theme("SPACING")@type, "SPACING")
  expect_equal(use.theme("TREE_COLORS")@type, "TREE_COLORS")
  expect_equal(use.theme("DATASET_STYLE")@type, "DATASET_STYLE")
  expect_equal(use.theme("LABELS")@type, "LABELS")
  expect_equal(use.theme("DATASET_TEXT")@type, "DATASET_TEXT")
  expect_equal(use.theme("DATASET_COLORSTRIP")@type, "DATASET_COLORSTRIP")
  expect_equal(use.theme("DATASET_BINARY")@type, "DATASET_BINARY")
  expect_equal(use.theme("DATASET_GRADIENT")@type, "DATASET_GRADIENT")
  expect_equal(use.theme("DATASET_HEATMAP")@type, "DATASET_HEATMAP")
  expect_equal(use.theme("DATASET_SYMBOL")@type, "DATASET_SYMBOL")
  expect_equal(use.theme("DATASET_EXTERNALSHAPE")@type, "DATASET_EXTERNALSHAPE")
  expect_equal(use.theme("DATASET_DOMAINS")@type, "DATASET_DOMAINS")
  expect_equal(use.theme("DATASET_SIMPLEBAR")@type, "DATASET_SIMPLEBAR")
  expect_equal(use.theme("DATASET_MULTIBAR")@type, "DATASET_MULTIBAR")
  expect_equal(use.theme("DATASET_BOXPLOT")@type, "DATASET_BOXPLOT")
  expect_equal(use.theme("DATASET_LINECHART")@type, "DATASET_LINECHART")
  expect_equal(use.theme("DATASET_PIECHART")@type, "DATASET_PIECHART")
  expect_equal(use.theme("DATASET_ALIGNMENT")@type, "DATASET_ALIGNMENT")
  expect_equal(use.theme("DATASET_CONNECTION")@type, "DATASET_CONNECTION")
  expect_equal(use.theme("DATASET_IMAGE")@type, "DATASET_IMAGE")
  expect_equal(use.theme("POPUP_INFO")@type, "POPUP_INFO")
})

test_that("use.theme type error", {
  expect_error(use.theme("COLLAPS"),"Unsupported")
})

test_that("use.theme style error", {
  expect_error(use.theme("COLLAPS",style = "cool"),"Unsupported")
})

test_that("create_unit works in COLLAPSE template", {
  tree <- system.file("extdata","tree_of_itol_templates.tree",package = "itol.toolkit")
  data("template_groups")
  data("template_parameters_count")
  group_names <- unique(template_groups$group)
  unit <- create_unit(data = group_names, key = "E001_collapse_1", type = "COLLAPSE", tree = tree)
  expect_equal(unit@type, "COLLAPSE")
  expect_error(create_unit(data = template_groups, key = "E001_collapse_1", type = "COLLAPSE", tree = tree),"input data")
})

test_that("create_unit works in PRUNE template", {
  tree <- system.file("extdata","tree_of_itol_templates.tree",package = "itol.toolkit")
  data("template_groups")
  select_note = c("theme_style","basic_plot")
  unit <- create_unit(data = select_note, key = "E002_prune_1", type = "PRUNE", tree = tree)
  expect_equal(unit@type, "PRUNE")
  expect_error(create_unit(data = template_groups, key = "E002_prune_1", type = "PRUNE", tree = tree),"input data")
})

test_that("create_unit works in SPACING template", {
  tree <- system.file("extdata","tree_of_itol_templates.tree",package = "itol.toolkit")
  data("template_parameters_count")
  df_values = data.frame(id = row.names(template_parameters_count), values = rowSums(template_parameters_count))
  unit <- create_unit(data = df_values, key = "E002_prune_1", type = "SPACING", tree = tree)
  expect_equal(unit@type, "SPACING")
  expect_error(create_unit(data = df_values$values, key = "E002_prune_1", type = "SPACING", tree = tree),"input data")
  names(df_values)[1] <- "template"
  expect_message(create_unit(data = df_values, key = "E002_prune_1", type = "SPACING", tree = tree),"id")
  df_values$addition <- NA
  expect_error(create_unit(data = df_values, key = "E002_prune_1", type = "SPACING", tree = tree),"input data")
})

test_that("create_unit works in TREE_COLORS template", {
  tree <- system.file("extdata","tree_of_itol_templates.tree",package = "itol.toolkit")
  data("template_groups")
  unit <- create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "range", tree = tree)
  expect_equal(unit@type, "TREE_COLORS")
  expect_error(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", tree = tree),"parameter")
  expect_message(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "range", tree = tree),"id")
  expect_message(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "range", tree = tree),"color")
  template_groups$subtype <- "range"
  expect_message(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "clade", tree = tree),"subtype")
  expect_error(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "clade", color = "red", tree = tree),"color")
  template_groups$subtype2 <- "ranges"
  expect_error(create_unit(data = template_groups,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "clade", tree = tree),"data column")
  data("template_parameters_count")
  expect_error(create_unit(data = template_parameters_count,key = "E006_tree_colors_1", type = "TREE_COLORS", subtype = "range", tree = tree),"data")
})
