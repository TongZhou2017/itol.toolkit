#' Read fasta file
#' @description Read the fasta format sequences file into data.frame
#' @importFrom ape read.FASTA
#' @param file input file in fasta format
#' @return a data frame with sequence id and sequence
#' @export
fa_read <- function(file) {
  dna <- ape::read.FASTA(file)
  df <- data.frame(seq_name=labels(dna),
                   sequence=sapply(as.character(dna), paste, collapse=""))
  df$seq_name <- as.character(df$seq_name)
  return(df)
}

#' Write fasta file
#' @description Write the fasta format sequences file from data.frame. (Version 0.0.0.9000)
#' @importFrom seqinr write.fasta
#' @param object data.frame format data
#' @param file input file in fasta format
#' @param id id col
#' @param seq seq col
#' @param append append at the end of an already existing file
#' @return No return value, only output a fasta file
#' @export
fa_write <- function(object, file, id = "seq_name", seq = "sequence", append = FALSE) {
  if (append) {
    seqinr::write.fasta(sequences = as.list(object[[seq]]), names = object[[id]], file.out = file, as.string = FALSE, open = "a")
  } else {
    seqinr::write.fasta(sequences = as.list(object[[seq]]), names = object[[id]], file.out = file, as.string = FALSE)
  }
}

#' head line
#'
#' @description Head line for templates
#' @param function_name parent function name
#' @return a character specifying the template type
#' @importFrom dplyr case_when
#' @export
head_line <- function(function_name) {
  head_line <- case_when(
    function_name == "itol_anno_label" ~ "LABELS\n",
    function_name == "itol_anno_strip" ~ "DATASET_COLORSTRIP\n",
    function_name == "itol_anno_bar" ~ "DATASET_SIMPLEBAR\n",
    function_name == "itol_anno_heatmap" ~ "DATASET_HEATMAP\n",
    function_name == "itol_anno_align" ~ "DATASET_ALIGNMENT\n",
    function_name == "itol_anno_style" ~ "DATASET_STYLE\n"
  )
  return(head_line)
}

#' correct_get_color
#' @description correct_get_color. (Version 0.0.0.9000)
#' @param str taxa string
#' @return a vector of colors
#' @import dplyr
#' @export
correct_get_color <- function(str) {
  str <- as.factor(str)
  color <- factor(str, levels(str), get_color(length(levels(str))))
  return(color)
}

#' get_color
#' @description get color, support max length 40
#' @param n level length of a vector
#' @param set a character specifying the palette set name. In dedault,
#' table2itol is setted. The following choices are possible: wsanderson.
#' @return a vector of colors
#' @importFrom wesanderson wes_palettes
#' @importFrom stats runif
#' @import ggsci
#' @import RColorBrewer
#' @export
get_color <- function(n=0,set="table2itol") {
  # Colour vectors collected by Jan P. Meier-Kolthoff.
  #

  COLOURS <- list(
    # Dark2; colour-blind-safe
    JMK01 = "#1b9e77",
    # Dark2; colour-blind-safe
    JMK02 = c("#d95f02", "#1b9e77"),
    # Dark2; colour-blind-safe
    JMK03 = c("#1b9e77", "#d95f02", "#7570b3"),
    # 4-class Paired; colour-blind-safe
    JMK04 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"),
    # 5-class Accent; print-friendly
    JMK05 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99"
    ),
    # 6-class Paired; print-friendly
    JMK06 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c"
    ),
    # 7-class Paired; print-friendly
    JMK07 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f"
    ),
    # Dark2; print-friendly
    JMK08 = c(
      "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
      "#66a61e", "#e6ab02", "#a6761d", "#666666"
    ),
    # 9-class Set1; print-friendly
    JMK09 = c(
      "#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"
    ),
    # 10-class Paired
    JMK10 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"
    ),
    # 11-class Paired
    JMK11 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
      "#ffff99"
    ),
    # 12-class Paired
    JMK12 = c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
      "#ffff99", "#b15928"
    ),
    ## from here on: iwanthue (all colours, hard)
    JMK13 = c(
      "#8393c7", "#8ad256", "#6a49c5", "#d2b351",
      "#cb55c3", "#4d4040", "#c4527c", "#57743d", "#d85439", "#7accb1",
      "#925136", "#ceb2ab", "#512f67"
    ),
    JMK14 = c(
      "#a2d1cd", "#5d39a8", "#71d14c", "#cb56c7",
      "#7ed094", "#4d4040", "#7077b8", "#c28b4c", "#cd9dae", "#c64a34",
      "#55868c", "#cccb51", "#b2436e", "#567137"
    ),
    JMK15 = c(
      "#92d4ad", "#6842c1", "#6ecf58", "#cb4ec2",
      "#55733d", "#4d4040", "#c99447", "#9083cb", "#c9d14f", "#4d2c63",
      "#cea4a2", "#d54f38", "#71a6bd", "#ca507f", "#823f33"
    ),
    JMK16 = c(
      "#76a5bd", "#bfdf44", "#cf4bab", "#66c95b",
      "#7c42c5", "#4d4040", "#7279ca", "#c27837", "#4b2a62", "#c7b956",
      "#cc8cb5", "#536e3b", "#d74746", "#84d3ae", "#893b42", "#cdb19a"
    ),
    JMK17 = c(
      "#823f35", "#77d952", "#6d44c4", "#78d5a1",
      "#cf4a70", "#4d4040", "#ca53bd", "#69923c", "#6d7fc4", "#d1d04e",
      "#532b63", "#d64d31", "#4b623d", "#ca96b7", "#78b5c2", "#ccbf9b",
      "#c58741"
    ),
    JMK18 = c(
      "#697bc5", "#5e9742", "#6641c0", "#7bdc57",
      "#c954c9", "#4d4040", "#4d2b62", "#73d6ac", "#d6493d", "#75adbe",
      "#c54883", "#526339", "#caca9b", "#7b332e", "#cfcf49", "#c89dc8",
      "#c58738", "#c78980"
    ),
    JMK19 = c(
      "#9e693f", "#9147d5", "#c9d747", "#9482d3",
      "#61913d", "#4d4040", "#6dd85e", "#d049a4", "#76d0b6", "#d5493c",
      "#6897bb", "#d7993d", "#553291", "#c7cb8a", "#472f5b", "#cd7993",
      "#496340", "#ccb8bc", "#7f2c3a"
    ),
    JMK20 = c(
      "#7295c1", "#d44b38", "#6ad14f", "#6a3bc0",
      "#cedb44", "#4d4040", "#77d192", "#cb4fc3", "#b1b85f", "#7772cc",
      "#d9973b", "#4f2b62", "#79d1cf", "#cc497b", "#4a6c2e", "#c990b5",
      "#752e30", "#d1c5ac", "#a26f47", "#537e71"
    ),
    JMK21 = c(
      "#90b5d9", "#d6532d", "#c84ccc", "#74d147",
      "#512d79", "#4d4040", "#6740c8", "#cace49", "#6b79d1", "#6ccc84",
      "#c8478c", "#74c4b8", "#cc4458", "#4f6781", "#cb9142", "#552443",
      "#c6cb97", "#82442d", "#c489c5", "#546d37", "#cb9896"
    ),
    JMK22 = c(
      "#392c51", "#4d4040", "#642c79", "#792d3b",
      "#6a3ec6", "#875b30", "#4f7231", "#547f72", "#d24637", "#6d71ce",
      "#d2497e", "#cd4fc8", "#6a8fbc", "#d88742", "#c78dc6", "#cc9795",
      "#c7af40", "#68cd55", "#72d4a6", "#9ecfd6", "#c9cb8f", "#c3de48"
    ),
    JMK23 = c(
      "#8ad93f", "#c749c4", "#5e8f3d", "#6639be",
      "#73d979", "#4d4040", "#d4ca4a", "#6c6ccc", "#d78c3b", "#6485b9",
      "#d24635", "#70d4ae", "#cc4279", "#cbcb99", "#4c295f", "#ce867e",
      "#793130", "#84cbd7", "#896c35", "#c27bbb", "#364e27", "#cab2cb",
      "#5b837b"
    ),
    JMK24 = c(
      "#ccc79a", "#6a42c7", "#d0a540", "#cc49c9",
      "#6dd755", "#4d4040", "#de5a26", "#7cc7d0", "#cc3f47", "#78d8a5",
      "#5e2d78", "#c9da51", "#6679d0", "#bf7348", "#c6b7d8", "#5f903c",
      "#c47ec5", "#6a5b29", "#ce4684", "#497359", "#772d38", "#c3858c",
      "#352444", "#5b7a9e"
    ),
    JMK25 = c(
      "#6ba43c", "#c74ace", "#cbe14b", "#6847cd",
      "#6ede53", "#4d4040", "#cbb248", "#592e82", "#d6842f", "#5e78c1",
      "#76dd99", "#c6438e", "#4b8047", "#cf4c67", "#7acdc4", "#d2472f",
      "#7ba5c4", "#79322f", "#c388cf", "#78662f", "#45294d", "#c8cd9d",
      "#3e5d4a", "#d08c6c", "#c698a9"
    ),
    JMK26 = c(
      "#73d991", "#b44adb", "#71d94d", "#cf4cb4",
      "#ccde4d", "#4d4040", "#ceae44", "#5a41c2", "#cdd09c", "#652e7a",
      "#83d7ce", "#dc4338", "#536e83", "#d34a79", "#5d9073", "#c68dc7",
      "#619339", "#85b1d7", "#da8340", "#6978cb", "#9d4533", "#34284e",
      "#d09e9e", "#732d41", "#364e25", "#866a38"
    ),
    JMK27 = c(
      "#363258", "#6ed853", "#5b3fc7", "#c9de43",
      "#b54ad9", "#4d4040", "#5c2c7e", "#b7d17b", "#cf4a83", "#6ed9a4",
      "#cd4450", "#8fd3d5", "#d74527", "#769ac1", "#d27d3f", "#6d75cf",
      "#d4af42", "#4f8c3b", "#d14eba", "#568778", "#c692c8", "#344625",
      "#d4c7a6", "#722e4c", "#c88988", "#7a3a25", "#86783a"
    ),
    JMK28 = c(
      "#7f3a27", "#71da53", "#c14bd4", "#55933d",
      "#626ad0", "#4d4040", "#623ac4", "#cbd943", "#542c79", "#c1d483",
      "#bc7fd0", "#6ad7a3", "#d84330", "#71bec7", "#ce7537", "#6f99d8",
      "#d5aa43", "#546586", "#7c7233", "#ce429f", "#3e6344", "#ce7d9f",
      "#2d1d38", "#c6b3ce", "#793151", "#bfcbae", "#d24566", "#c8927d"
    ),
    JMK29 = c(
      "#cdc2c2", "#663dc8", "#76dd51", "#c64ece",
      "#cfda49", "#4d4040", "#549e3f", "#7577da", "#d3522e", "#7cd6ce",
      "#d4425b", "#77de9a", "#542a7e", "#d1d395", "#321e3d", "#d74a98",
      "#95963d", "#586095", "#db9a3e", "#77abd9", "#8b3c67", "#639575",
      "#d08982", "#456129", "#ca92cc", "#896134", "#597984", "#742c28",
      "#283a28"
    ),
    JMK30 = c(
      "#31223c", "#bbe141", "#c94edb", "#65d559",
      "#8b3899", "#4d4040", "#613ec8", "#df9b36", "#6e75d5", "#c16c39",
      "#402a74", "#cfc248", "#da47a4", "#63d6ad", "#d94330", "#6abccd",
      "#c58181", "#617fae", "#7f2f2c", "#b5cfb8", "#833b65", "#b5d888",
      "#cc88cb", "#4e8a3b", "#d6466a", "#476d58", "#d2b284", "#544320",
      "#c9b6d0", "#867c36"
    ),
    JMK31 = c(
      "#913d83", "#ced242", "#6643d0", "#79d949",
      "#c249d4", "#4d4040", "#db45a4", "#68dc88", "#3a1f4f", "#c3d483",
      "#532e8e", "#da983e", "#6d79d5", "#9b4b29", "#d085d5", "#8b7d3b",
      "#c9a0c0", "#54913d", "#dc4b32", "#72d4b1", "#8f3e58", "#90d0d8",
      "#592720", "#d2c7a9", "#21262c", "#d64769", "#3b4f25", "#6ea2cf",
      "#cd887a", "#5c6089", "#568477"
    ),
    JMK32 = c(
      "#8f8b38", "#663cc8", "#6bd546", "#c74cce",
      "#b1d773", "#4d4040", "#c6e03a", "#59287c", "#5edb86", "#d14592",
      "#7ad9b1", "#da4627", "#719cd8", "#dc973a", "#6e71d7", "#dbc348",
      "#ca84c8", "#4c8b3a", "#d5445a", "#84ccd6", "#7f3353", "#d3c99f",
      "#2e1c38", "#ca7442", "#5a558b", "#803325", "#537286", "#cc8585",
      "#314826", "#cab3cc", "#7e6136", "#618d75"
    ),
    JMK33 = c(
      "#d64e9e", "#6cd54c", "#dd49d1", "#c8dd41",
      "#a152dd", "#4d4040", "#5139c2", "#ceaa3b", "#432d7c", "#c6d179",
      "#8f379a", "#70d68c", "#d9432f", "#6ad5be", "#d5416a", "#76c2d7",
      "#d87a71", "#6a75d5", "#836834", "#c988d1", "#598939", "#7a3260",
      "#bed3b3", "#8f372e", "#6082b3", "#d47c35", "#312749", "#d4ac8b",
      "#314825", "#cab9d7", "#4b211f", "#ad788b", "#568275"
    ),
    JMK34 = c(
      "#d8436c", "#653cc7", "#b4dc41", "#d143d0",
      "#5fd857", "#4d4040", "#a4db84", "#c64496", "#6adcad", "#de4830",
      "#6aa3d9", "#d98731", "#6271d1", "#dec841", "#b062cd", "#528e36",
      "#c28acd", "#675b2c", "#cbb7d3", "#a53332", "#528089", "#532878",
      "#d9d393", "#2a1e3c", "#8ed4d3", "#834629", "#5e5e8a", "#a08e3c",
      "#2b482a", "#d78763", "#619470", "#c87b8d", "#702944", "#c3a994"
    ),
    JMK35 = c(
      "#72d4cf", "#ccdf3e", "#5533c1", "#70d951",
      "#ac42d6", "#4d4040", "#6d66dc", "#b9c866", "#562a84", "#71da99",
      "#db43c7", "#518f39", "#d04497", "#314826", "#bc6cc9", "#5d8b74",
      "#d2416d", "#72abd3", "#dd461f", "#6078c6", "#d7ab3b", "#c49ad6",
      "#7d6b2f", "#cab8c4", "#3c1a20", "#c8ddb6", "#312652", "#cfb182",
      "#7c3463", "#c98271", "#576782", "#d24243", "#cb7a99", "#82372d",
      "#cf7734"
    ),
    JMK36 = c(
      "#6ade4b", "#6344d3", "#7bdc86", "#b746d4",
      "#65a234", "#4d4040", "#dbc941", "#552c93", "#bee148", "#dc3fb4",
      "#62d7b4", "#903a7e", "#4a8245", "#cf74d0", "#da993a", "#3e255f",
      "#c0d3b2", "#291d2d", "#cdce7e", "#752c41", "#7dcbd6", "#c43c44",
      "#669bcf", "#de4e28", "#5b5e83", "#c97449", "#bd92d0", "#847933",
      "#d7417a", "#558279", "#d07d92", "#364525", "#ceb9d0", "#763d23",
      "#6872d2", "#be9880"
    ),
    JMK37 = c(
      "#645b8e", "#80dc40", "#4f2ea4", "#69dc7b",
      "#d848cd", "#4d4040", "#8548da", "#c7d84e", "#96368e", "#afd995",
      "#d54227", "#61d9b9", "#db4187", "#4a9339", "#cd83d6", "#7a8431",
      "#6870d5", "#e3bc3b", "#6b9bd7", "#d87935", "#6fbfcf", "#cd3e50",
      "#c3d8c8", "#772e29", "#dbc38b", "#3f2267", "#bf9340", "#cab1d6",
      "#304726", "#b2918d", "#2a1f35", "#d5816f", "#5e8c6b", "#c77192",
      "#497080", "#7d592d", "#732d52"
    ),
    JMK38 = c(
      "#cf8ad0", "#74e042", "#b946da", "#5be080",
      "#5834c1", "#4d4040", "#d248bb", "#59a434", "#8064d4", "#b4dc4e",
      "#893876", "#96db99", "#d9478a", "#499052", "#627bcf", "#dfd238",
      "#47277a", "#908f39", "#79a2d8", "#d79234", "#4c7788", "#df502c",
      "#625984", "#d7d27b", "#2e1d3b", "#6bdac4", "#d34557", "#6a8b73",
      "#9e4427", "#cfb5cd", "#78562e", "#7cc6d5", "#26392b", "#cdcfb2",
      "#702735", "#bd7984", "#405924", "#d59571"
    ),
    JMK39 = c(
      "#8b308f", "#74dd41", "#6939ca", "#cce346",
      "#d545d2", "#4d4040", "#b271dd", "#e39b39", "#5050bc", "#cabc46",
      "#3a1f64", "#5cde7e", "#d9428e", "#57a56d", "#d63949", "#76dfc2",
      "#7e3052", "#b7e28f", "#d286c6", "#66a234", "#6d83d8", "#d65629",
      "#76c3d2", "#843326", "#6aa0d5", "#9c762c", "#5f5488", "#d48e70",
      "#4a6a81", "#d36778", "#466b2c", "#b28491", "#273825", "#c1b47a",
      "#301b31", "#d0d2bd", "#6c552d", "#c9b8d8", "#5f8675"
    ),
    JMK40 = c(
      "#3c2b5d", "#dee032", "#ab48d5", "#5bd749",
      "#db49c6", "#4d4040", "#5c42d0", "#a4e040", "#462687", "#d8b136",
      "#8d3989", "#60d076", "#d7468f", "#63d8b5", "#de4528", "#77c7d6",
      "#d13a55", "#5f8c7b", "#ce88d5", "#759b31", "#696ecd", "#de8739",
      "#6f9ad6", "#b75738", "#aadc90", "#946d89", "#d0dc6a", "#2c1a25",
      "#c6d8bc", "#782849", "#ceb977", "#283f27", "#d9798c", "#447c3d",
      "#ceb8d4", "#635b2d", "#c79783", "#733426", "#476682", "#98762e"
    )
  )
  if(set == "table2itol"){
    return(COLOURS[[n]])
  }
  set_names <- c("table2itol")
  if(set == "wesanderson"){
    COLOURS <- list()
    colors_vector <- unique(unlist(wesanderson::wes_palettes))
    colors_vector <- colors_vector[which(!colors_vector%in%c("#1E1E1E",
                                                             "#4E2A1E",
                                                             "#0C1707",
                                                             "#000000",
                                                             "#1C1718",
                                                             "#39312F",
                                                             "#0F0D0E",
                                                             "#29211F",
                                                             "#24281A",
                                                             "#273046",
                                                             "#35274A",
                                                             "#550307"))]
    for (i in 1:length(colors_vector)) {
      indexs <- sample(1:length(colors_vector),n,replace=F)
      COLOURS[[i]] <- colors_vector[indexs]
    }
    return(COLOURS[[n]])
  }
  set_names <- c(set_names,"wesanderson")
  set_main <- stringr::str_remove(set, "_.*$")
  ggsci_db<-utils::getFromNamespace("ggsci_db", "ggsci")
  if(set_main %in% names(ggsci_db)){
    if(grepl("_",set)){
      set_type <- unlist(strsplit(set,"_"))
      colors <- ggsci_db[[set_type[1]]][[set_type[2]]]
    }else{
      colors <- ggsci_db[[set]][[1]]
    }
    if(n > length(colors)){
      warning("The pattern length is shorter than n. Appending default colors to
              make the pattern length same with n.")
      colors <- c(colors,COLOURS[[n-length(colors)]])
    }
    if(n < length(colors)){
      warning("The pattern length is longer than n. The length is cutting as
              same as n.")
      colors <- colors[1:n]
    }
    return(colors)
  }
  set_names <- c(set_names,names(ggsci_db))
  if(set %in% rownames(RColorBrewer::brewer.pal.info)){
    n_max <- RColorBrewer::brewer.pal.info[set,]$maxcolors
    if(n > n_max){
      warning("The pattern length is shorter than n. Appending default colors to
              make the pattern length same with n.")
      colors <- RColorBrewer::brewer.pal(n=n_max,name = set)
      colors <- c(colors,COLOURS[[n-n_max]])
    }
    if(n < n_max){
      warning("The pattern length is longer than n. The length is cutting as
              same as n.")
      colors <- RColorBrewer::brewer.pal(n=n,name = set)
    }
    if(n == n_max){
      colors <- RColorBrewer::brewer.pal(n=n,name = set)
    }
    return(colors)
  }
  set_names <- c(set_names,rownames(RColorBrewer::brewer.pal.info))
  if(set == "ls"){
    return(set_names)
  }
}

#' Paste rows
#'
#' @description Paste rows group by key column
#' @param df input data frame
#' @return a data frame with pasted row by same id
#' @import dplyr
#' @export
unite_rows <- function(df) {
  names <- names(df)
  vars <- paste0("V", c(1:(length(names) - 1)))
  names(df) <- c("id", vars)
  df2 <- eval(parse(text = paste0("df %>% group_by(id) %>% summarize(", paste0(vars, " = paste(", vars, ",collapse = '|')", sep = ",", collapse = ""), ")")))
  names(df2) <- names
  df2 <- as.data.frame(df2)
  return(df2)
}

#' Get file name
#' @description Get file name from string
#' @param str str
#' @param with_ext with ext or not
#' @param keep_dir keep file dir or not
#' @return a character specifying the file name
#' @import stringr
#' @export

file_get_name <- function(str, with_ext = TRUE, keep_dir = FALSE) {
  dir <- file_get_dir(str)
  if(dir=="./"){
    str <- str
  }else{
    str <- stringr::str_extract(str, "/[^/]*$")
    str <- substr(str, 2, nchar(str))
  }
  if (!with_ext) {
    ext <- stringr::str_extract(str, "\\.[^.]*$")
    str <- substr(str, 1, nchar(str) - nchar(ext))
  }
  if (keep_dir) {
    str <- paste0(dir, str)
  }
  return(str)
}

#' Get file dir
#' @description Get file dir from string
#' @param str str
#' @param up up dir
#' @return a character specifying the dir path
#' @import stringr
#' @export

file_get_dir <- function(str, up = FALSE) {
  if (up) {
    str <- stringr::str_extract(str, "^.*[/]")
    str <- stringr::str_remove(str, "/$")
    str <- stringr::str_extract(str, "^.*[/]")
  } else {
    str <- stringr::str_extract(str, "^.*[/]")
  }
  if(is.na(str)){
    str <- "./"
  }
  return(str)
}

#' Search tree file
#' @description Search Newick format tree file in dir
#' @param dir a path with tree file and other template files
#' @param n 'first', 'last', 'all'
#' @param method sort by 'mtime', 'ctime', 'atime', 'character'
#' @param max_size limit file size to accelerate searching
#' @importFrom ape read.tree
#' @importFrom stringr str_detect
#' @importFrom utils head
#' @importFrom utils tail
#' @return a vector of characters specifying the file name
#' @export
search_tree_file <- function(dir=getwd(),
                             n="first",
                             method="mtime",
                             max_size = 10240){
  files = setdiff(list.files(path = dir,
                             full.names = TRUE),
                  list.dirs(path = dir,
                            recursive = FALSE,
                            full.names = TRUE))
  results <- c()
  for (file in files) {
    if(file.size(file) < max_size){
      con <- file(file,"r")
      first_line <- readLines(con,n=1)
      close(con)
      if(length(first_line)==0) next
      if(suppressWarnings(stringr::str_detect(first_line,'^#NEXUS|^\\('))){
        results <- c(results,file)
      }
    }
  }
  df <- file.info(results)
  df$fname <- file_get_name(rownames(df))
  if(method!='character'){
    suppressWarnings(df <- df %>% arrange_(method))
  }
  if(n == 'first'){
    df <- df %>% head(1)
  }
  if(n == 'last'){
    df <- df %>% tail(1)
  }
  return(rownames(df))
}

#' Correct type parameter
#'
#' @description Correct type parameter input by string similarity and synonym mapping.
#' @param str A character string representing a type name to correct.
#' @importFrom stringdist stringsim
#' @return A character string of the corrected type name.
#' @export
#' @examples
#' correct_type("line")
correct_type <- function(str) {
    vec <- c('COLLAPSE', 'PRUNE', 'SPACING', 'TREE_COLORS', 'DATASET_STYLE', 'LABELS',
             'DATASET_TEXT', 'DATASET_COLORSTRIP', 'DATASET_BINARY', 'DATASET_GRADIENT',
             'DATASET_HEATMAP', 'DATASET_SYMBOL', 'DATASET_EXTERNALSHAPE', 'DATASET_DOMAINS',
             'DATASET_SIMPLEBAR', 'DATASET_MULTIBAR', 'DATASET_BOXPLOT', 'DATASET_LINECHART',
             'DATASET_PIECHART', 'DATASET_ALIGNMENT', 'DATASET_CONNECTIONS', 'DATASET_IMAGE', 'POPUP_INFO')
    if(str %in% vec){
      return(str)
    }else{

    input_str_lower <- tolower(str)

    vec_lower <- tolower(vec)
    vec_lower_no_prefix <- gsub("^dataset_", "", vec_lower)

    best_match <- NULL
    best_match_score <- -1

    for (i in 1:length(vec)) {
      target_str <- vec[i]
      target_str_no_prefix <- vec_lower_no_prefix[i]

      match_score_with_prefix <- longest_continuous_match(input_str_lower, target_str)
      match_score_no_prefix <- longest_continuous_match(input_str_lower, target_str_no_prefix)

      if (match_score_with_prefix > best_match_score) {
        best_match_score <- match_score_with_prefix
        best_match <- target_str
      }
      if (match_score_no_prefix > best_match_score) {
        best_match_score <- match_score_no_prefix
        best_match <- target_str
      }
    }

    return(best_match)

    }
}

#' Calculate longest continuous match between input and target strings
#'
#' @description This function calculates the longest continuous matching substring between an input string and a target string.
#' @param input_str A character string to match against the target string.
#' @param target_str A character string to compare with the input string.
#' @return An integer value representing the longest continuous match.
#' @export
longest_continuous_match <- function(input_str, target_str) {
  max_match_length <- 0
  input_str <- gsub("[^a-zA-Z]", "", input_str)
  target_str <- gsub("[^a-zA-Z]", "", target_str)

  for (i in 1:(nchar(target_str) - nchar(input_str) + 1)) {
    substring_target <- substr(target_str, i, i + nchar(input_str) - 1)

    match_length <- 0
    for (j in 1:nchar(input_str)) {
      if (substr(input_str, j, j) == substr(substring_target, j, j)) {
        match_length <- match_length + 1
      } else {
        break
      }
    }

    max_match_length <- max(max_match_length, match_length)
  }
  return(max_match_length)

}


