.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "",
    "If you use the itol.toolkit package in published research, please cite:",
    "Zhou, T., Xu, K., Zhao, F., Liu, W., Li, L., Hua, Z., & Zhou, X. (2023).",
    "itol.toolkit accelerates working with iTOL (Interactive Tree of Life) by an",
    "automated generation of annotation files. Bioinformatics, 39(6), btad339.",
    sep = "\n"
  )
  packageStartupMessage(msg)
}
