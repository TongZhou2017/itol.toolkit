#' Color distance
#' @description calculate distance between two color in hex or rgb format. rgb
#' maxColorValue = 255.
#' @param color_1 color 1 in hex or rgb format
#' @param color_2 color 2 in hex or rgb format
#' @importFrom grDevices col2rgb
#' @return a distance number
#' @export
color_distance <- function(color_1,color_2){
  if(grepl("^#",color_1[1])){
    color_1 <- col2rgb(color_1)
  }
  if(grepl("^#",color_2[1])){
    color_2 <- col2rgb(color_2)
  }
  R1 <- color_1[1]
  G1 <- color_1[2]
  B1 <- color_1[3]
  R2 <- color_2[1]
  G2 <- color_2[2]
  B2 <- color_2[3]
  Rmean <- (R1 + R2)/2
  R <- R1 - R2
  G <- G1 - G2
  B <- B1 - B2
  dis <- sqrt((2+Rmean/256)*(R**2)+4*(G**2)+(2+(255-Rmean)/256)*(B**2))
  return(dis)
}

#' Generate gradient colors
#' @description generate a vector of gradient colors by start, mid, and end
#' colors.
#' @param n the length of vector
#' @param start start color in hex format
#' @param mid mid color in hex format, default is null.
#' @param end end color in hex format, default is white.
#' @importFrom grDevices colorRampPalette
#' @return a vector of gradient colors
#' @export
gradient_color <- function(n,start,mid=NULL,end="#FFFFFF"){
  if(is.null(mid)){
    if(color_distance(end,"#FFFFFF")<10){
      fun_colors <- colorRampPalette(c(start, end))
      colors <- fun_colors(6)
      end <- colors[5]
    }
    fun_colors <- colorRampPalette(c(start, end))
    colors <- fun_colors(n)
  }else{
    if(color_distance(end,"#FFFFFF")<10){
      fun_colors <- colorRampPalette(c(mid, end))
      colors <- fun_colors(6)
      end <- colors[5]
    }
    fun_colors_1 <- colorRampPalette(c(start, mid))
    fun_colors_2 <- colorRampPalette(c(mid, end))
    colors <- c(fun_colors_1(n/2),fun_colors_2(n/2))
  }
  return(colors)
}

#' Sort colors
#' @description sort colors by similarity
#' @param colors a vector of colors in hex format
#' @param root sort from root color
#' @param rev sort order by similarity(default) or difference
#' @param plot preview color order
#' @importFrom ape as.phylo
#' @importFrom ape root
#' @importFrom stats as.dist
#' @return a vector of sorted colors
#' @export
sort_color <- function(colors,root=NULL,rev=FALSE,plot=FALSE){
  colors <- as.character(colors)
  if(!is.null(root)){
    colors <- c(colors,root)
  }
  mtx <- matrix(0,ncol=length(colors),nrow=length(colors))
  for (i in 1:length(colors)){
    for (j in 1:length(colors)){
      mtx[i,j] <- color_distance(color_1=colors[i],colors[j])
    }
  }
  tree <- as.phylo(hclust(as.dist(mtx),method="complete"))
  if(!is.null(root)){
    tree <- root(tree, outgroup = as.character(length(colors)), resolve.root = TRUE)
  }
  if(plot){
    plot(tree,tip.col=colors)
  }
  is_tip <- tree$edge[,2] <= length(tree$tip.label)
  ordered_tips <- tree$edge[is_tip, 2]
  old_order <- tree$tip.label[ordered_tips]
  new_colors <- colors[order(old_order)]
  if(!is.null(root)){
    new_colors <- new_colors[-which(old_order == length(colors))]
  }
  return(new_colors)
}
