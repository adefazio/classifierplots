

#' @title notation_key_plot
#' @description Produces some definitions as a grid.
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @export
notation_key_plot <- function() {
  img <- png::readPNG(system.file("img", "notation.png", package="classifierplots"))
  g <- grid::rasterGrob(img, interpolate=TRUE)

  return(g)
}
