#' Data frame tree
#' @description build a tree by row and column names of a data frame.
#' @param df input data frame
#' @param main col(default) or row
#' @param order none or value
#' @param sep default '_'
#' @export
df_tree <- function(df, main='col', order='none', sep='_'){
  # check

  # main
  name_list <- list(col = colnames(df),
                    row = rownames(df))
  ## order

  ## paste
  group_1 <- rep(unlist(name_list[main]),each=length(unlist(name_list[!names(name_list) %in% main])))
  group_2 <- rep(unlist(name_list[!names(name_list) %in% main]),length(unlist(name_list[main])))
  tip_label <- paste0(group_1,sep,group_2)

  df_t <- data.frame(as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_1)),as.numeric(factor(group_2))*0.01)
  rownames(df_t) <- tip_label

  dist <- dist(df_t)
  clust <- hclust(dist)
  tree <- ape::as.phylo(clust)
  # report
  tree
}

#' Vector tree
#' @description build a tree by a vector of characters.
#' @param vector input vector
#' @param branch_length branch length, default 1.
#' @export
vector_tree <- function(vector, branch_length=1){
  newick_str <- paste0(vector[1], ":",branch_length)  # Start with the first element
  for (i in 2:length(vector)) {
    newick_str <- paste0("(", newick_str, ",", vector[i], ":",branch_length,"):",branch_length)  # Add branch length to internal nodes
  }
  newick_str <- paste0(newick_str, ";")
  # print(newick_str)
  # Convert Newick string to phylo object
  tree <- read.tree(text = newick_str)

  tree <- ape::read.tree(text = newick_str)
  # report
  tree
}
