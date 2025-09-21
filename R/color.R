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

#' Darken color while preserving hue and saturation
#' @description Darken a color by proportionally reducing RGB values while maintaining the color's hue and saturation characteristics.
#' @param color input color in hex format
#' @param factor darkening factor (0-1), where 0 is no darkening and 1 is maximum darkening
#' @param method method for darkening: "proportional" (default) or "gradient"
#' @importFrom grDevices col2rgb
#' @return darkened color in hex format
#' @export
darken_color <- function(color, factor = 0.4, method = "proportional"){
  if(method == "proportional"){
    # Convert to RGB
    rgb_vals <- col2rgb(color)
    
    # Calculate darkening: reduce each RGB component proportionally
    # factor = 0 means no change, factor = 1 means complete darkening (black)
    darken_ratio <- 1 - factor
    new_rgb <- round(rgb_vals * darken_ratio)
    
    # Convert back to hex
    hex_color <- sprintf("#%02X%02X%02X", new_rgb[1], new_rgb[2], new_rgb[3])
    return(hex_color)
  } else if(method == "gradient"){
    # Use gradient_color with a dark target color
    # Calculate a dark version of the original color
    rgb_vals <- col2rgb(color)
    
    # Create a dark target by reducing all RGB components to 20% of original
    dark_target <- round(rgb_vals * 0.2)
    dark_hex <- sprintf("#%02X%02X%02X", dark_target[1], dark_target[2], dark_target[3])
    
    # Generate gradient and take the factor position
    gradient_colors <- gradient_color(10, color, end = dark_hex)
    gradient_position <- round(factor * 9) + 1  # factor 0.4 -> position 5
    gradient_position <- min(gradient_position, 10)  # Ensure not exceeding bounds
    
    return(gradient_colors[gradient_position])
  } else if(method == "enhanced"){
    # Enhanced method: for neutral colors, use a saturated target instead of black
    rgb_vals <- col2rgb(color)
    
    # Calculate saturation (difference between max and min RGB components)
    max_rgb <- max(rgb_vals)
    min_rgb <- min(rgb_vals)
    saturation <- max_rgb - min_rgb
    
    # If the color is quite neutral (low saturation), use gradient to a saturated dark color
    if(saturation < 60){
      # Find the dominant color component
      dominant_idx <- which.max(rgb_vals)
      
      # Create a saturated dark target color
      target_rgb <- c(40, 40, 40)  # Base dark color
      target_rgb[dominant_idx] <- 120  # Make the dominant color component brighter
      
      # Use gradient_color to create a smooth transition
      target_hex <- sprintf("#%02X%02X%02X", target_rgb[1], target_rgb[2], target_rgb[3])
      gradient_colors <- gradient_color(10, color, end = target_hex)
      
      # Take the appropriate position based on factor
      gradient_position <- round(factor * 9) + 1
      gradient_position <- min(gradient_position, 10)
      
      return(gradient_colors[gradient_position])
    } else {
      # For saturated colors, use normal proportional darkening
      darken_ratio <- 1 - factor
      new_rgb <- round(rgb_vals * darken_ratio)
      hex_color <- sprintf("#%02X%02X%02X", new_rgb[1], new_rgb[2], new_rgb[3])
      return(hex_color)
    }
  }
}
