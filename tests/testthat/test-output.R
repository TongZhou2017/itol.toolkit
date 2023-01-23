test_that("write_unit error", {
  tree <- system.file("extdata",
                     "tree_of_itol_templates.tree",
                     package = "itol.toolkit")
  hub <- create_hub(tree = tree)
  data("template_groups")
  df_group <- data.frame(id = unique(template_groups$group),
                         data = unique(template_groups$group))
  unit <- create_unit(data = df_group,
                      key = "Quickstart",
                      type = "DATASET_COLORSTRIP",
                      tree = tree)
  expect_error(write_unit(hub))
  expect_error(write_unit("unit"))
})

test_that("write_hub error", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  data("template_groups")
  df_group <- data.frame(id = unique(template_groups$group),
                         data = unique(template_groups$group))
  unit <- create_unit(data = df_group,
                      key = "Quickstart",
                      type = "DATASET_COLORSTRIP",
                      tree = tree)
  expect_error(write_hub(unit))
  expect_error(write_hub("hub"))
})

test_that("write_raw error", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  hub <- create_hub(tree = tree)
  data("template_groups")
  df_group <- data.frame(id = unique(template_groups$group),
                         data = unique(template_groups$group))
  unit <- create_unit(data = df_group,
                      key = "Quickstart",
                      type = "DATASET_COLORSTRIP",
                      tree = tree)
  expect_error(write_raw(hub))
  expect_error(write_raw(unit))
})
