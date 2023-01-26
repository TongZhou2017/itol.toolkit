test_that("learn_type works", {
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
  file <- tempfile()
  write_unit(unit,file)
  expect_equal(learn_type(file), "DATASET_COLORSTRIP")
})

test_that("learn_df works", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  sub_df <- learn_df(tree,node=TRUE,tip=TRUE)
  phylo <- ape::read.tree(tree)
  sub_df_2 <- learn_df(phylo,node=TRUE,tip=TRUE)
  expect_equal(length(sub_df),2)
  expect_equal(length(sub_df_2),2)
})

test_that("line_clean works", {
  strs <- c("#comment","DATA")
  expect_equal(line_clean(lines=strs),"DATA")
})

test_that("learn_separator works", {
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
  file <- tempfile()
  write_unit(unit,file)
  expect_equal(learn_separator(file = file), "\t")
})

test_that("learn_line works", {
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
  file <- tempfile()
  write_unit(unit,file)
  lines <- line_clean(file=file)
  sep = learn_separator(file = file)
  expect_equal(learn_line(lines = lines, param = "STRIP_WIDTH", sep = sep), "25")
})

test_that("learn_theme_strip_label works", {
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
  file <- tempfile()
  write_unit(unit,file)
  lines <- line_clean(file=file)
  sep = learn_separator(file = file)
  expect_equal(learn_theme_strip_label(lines,sep)$width,"25")
})

test_that("learn_field works", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  df_frequence <- data.table::fread(system.file("extdata",
                                                "templates_frequence.txt",
                                                package = "itol.toolkit"))
  unit <- create_unit(data = df_frequence,
                      key = "Quickstart",
                      type = "DATASET_HEATMAP",
                      tree = tree)
  file <- tempfile()
  write_unit(unit,file)
  lines <- line_clean(file=file)
  sep = learn_separator(file = file)
  expect_equal(learn_field(lines,sep)$labels[1],"10.1016/j.jhazmat.2022.129230")
})

test_that("learn_profile works", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  df_frequence <- data.table::fread(system.file("extdata",
                                                "templates_frequence.txt",
                                                package = "itol.toolkit"))
  unit <- create_unit(data = df_frequence,
                      key = "Quickstart",
                      type = "DATASET_HEATMAP",
                      tree = tree)
  file <- tempfile()
  write_unit(unit,file)
  lines <- line_clean(file=file)
  sep = learn_separator(file = file)
  expect_equal(learn_profile(lines,sep)$name,"Quickstart")
})

test_that("learn_theme_label works", {
  tree <- system.file("extdata",
                      "tree_of_itol_templates.tree",
                      package = "itol.toolkit")
  tab_tmp <- fread(system.file("extdata","parameter_groups.txt",package = "itol.toolkit"))
  tab_id_group <- tab_tmp[,c(1,2)]
  tab_tmp <- tab_tmp[,-c(1,2)]
  tab_tmp_01 <- convert_01(object = tab_tmp)
  tab_tmp_01 <- cbind(tab_id_group,tab_tmp_01)

  order <- c("type","separator","profile","field","common themes","specific themes","data")

  tab_tmp_01_long <- tab_tmp_01 %>% tidyr::gather(key = "variable",value = "value",c(-parameter,-group))

  template_start_group <- tab_tmp_01_long %>% group_by(group,variable) %>% summarise(sublen = sum(value)) %>% tidyr::spread(key=variable,value=sublen)
  template_start_group$group <- factor(template_start_group$group,levels = order)
  template_start_group <- template_start_group %>% arrange(group)
  start_group <- data.frame(Var1 = template_start_group$group, Freq = apply(template_start_group[,-1], 1, max))
  start_group$start <- 0
  for (i in 2:nrow(start_group)) {
    start_group$start[i] <- sum(start_group$Freq[1:(i-1)])
  }
  template_start_group[template_start_group == 0] <- NA
  template_end_group <- template_start_group[,2:(ncol(template_start_group)-1)] + start_group$start
  template_end_group <- data.frame(group = order,template_end_group)
  template_end_group_long <- template_end_group %>% tidyr::gather(key = "variable",value = "value",-group)
  names(template_end_group_long)[3] <- "end"
  template_end_group_long$start <- rep(start_group$start,length(unique(template_end_group_long$variable)))
  template_end_group_long <- template_end_group_long %>% na.omit()
  template_end_group_long$length <- sum(start_group$Freq)
  template_end_group_long <- template_end_group_long[,c(2,5,4,3,1)]
  template_end_group_long$group <- factor(template_end_group_long$group,levels = order)

  unit <- create_unit(data = template_end_group_long,
                      key = "Quickstart",
                      type = "DATASET_DOMAINS",
                      tree = tree)
  file <- tempfile()
  write_unit(unit,file)
  lines <- line_clean(file=file)
  sep = learn_separator(file = file)
  expect_equal(learn_theme_label(lines,sep)$top,"0")
})
