#' Path to custom sticker fonts
#' @export
fonts_path <- function() {
  list.files(
    system.file("fonts", package = "rTRhexNG"),
    "[.]otf$", recursive = TRUE, full.names = TRUE
  )
}

#' Install custom sticker fonts
#' @export
install_fonts <- function() {
  dir.create('~/.fonts', showWarnings = FALSE)
  file.copy(rTRhexNG::fonts_path(), "~/.fonts")
  system('fc-cache -f ~/.fonts')
}
