.onAttach <- function(libname, pkgname) {
  header <- cli::rule(
    left = cli::style_bold("Loading"),
    right = paste0("itol.toolkit ", utils::packageVersion("itol.toolkit"))
  )

  p1 <- "If you use the itol.toolkit package in published research, please cite:"
  p2 <- "Zhou, T., Xu, K., Zhao, F., Liu, W., Li, L., Hua, Z., & Zhou, X. (2023). itol.toolkit accelerates working with iTOL (Interactive Tree of Life) by an automated generation of annotation files. Bioinformatics, 39(6), btad339."

  # Display the formatted message
  cli::cli_text("\n")
  packageStartupMessage(header)
  cli::cli_text("\n")
  cli::cli_text(p1)
  cli::cli_text("\n")
  cli::cli_text(cli::col_blue(p2))
}
