test_that("create_hub works", {
  tree <- system.file("extdata", "tree_of_itol_templates.tree", package = "itol.toolkit")
  hub <- create_hub(tree = tree)
  expect_equal(hub@tree$main$Nnode,22)
})
