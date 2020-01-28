# ShinyApp


if (file.access(.libPaths()[1], 2) < 0) {
  dir.create("~/.r-library", showWarnings = FALSE)
  .libPaths(c("~/.r-library", .libPaths()))
}
install.packages(".", repos = NULL)

# install custom font
rTRhexNG::install_fonts()

rTRhexNG::rTRhexNG_app()
