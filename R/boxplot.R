
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

geom_ambiboxplot <- function(mapping = NULL, data = NULL,
                         stat = "boxplot", position = "dodge",
                         ...,
                         width=0.3,
                         outlier.colour = NULL,
                         outlier.color = NULL,
                         outlier.shape = 19,
                         outlier.size = 0.8,
                         outlier.stroke = 0.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAmbiBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      na.rm = na.rm,
      width = width,
      ...
    )
  )
}

#' @importFrom grid grobTree
GeomAmbiBoxplot <- ggproto("GeomAmbiBoxplot", Geom,
    setup_data = function(data, params) {
        # Set width to a default if it is not part of data.
        data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

        if (!is.null(data$outliers)) {
            suppressWarnings({
              out_min <- vapply(data$outliers, min, numeric(1))
              out_max <- vapply(data$outliers, max, numeric(1))
        })

        data$ymin_final <- pmin(out_min, data$ymin)
        data$ymax_final <- pmax(out_max, data$ymax)
        }

        data$xmin <- data$x - data$width / 2.0
        data$xmax <- data$x + data$width / 2.0
        data$barmin <- data$x - data$width / 3.0
        data$barmax <- data$x + data$width / 3.0

        #print("setup_data end")
        #browser()
        data
    },
  draw_group = function(self, data, panel_scales, coord, fatten = 2,
                        outlier.colour = NULL, outlier.shape = 19,
                        outlier.size = 0.8, outlier.stroke = 0.5) {
    #print("draw group called")
    #browser()
    common <- data.frame(
      colour = alpha(data$fill, data$alpha),
      size = data$size,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      alpha = 1.0,
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x = data$x,
      xend = data$x,
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = common$fill,
      colour = common$fill,
      common
    )

    box <- data.frame(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      ymax = data$upper,
      alpha = data$alpha,
      common
    )
    
    crossbar <- data.frame(
      x = data$barmin,
      xend = data$barmax,
      y = data$middle,
      yend = data$middle,
      colour = data$colour,
      size = data$size,
      group = data$group,
      alpha = 1.0
    )

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = outlier.colour %||% data$fill[1],
        alpha = 1.0,
        stringsAsFactors = FALSE
      )
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_scales, coord)
    } else {
      outliers_grob <- NULL
    }

    ggname("geom_ambiboxplot", grobTree(
      outliers_grob,
      GeomSegment$draw_panel(whiskers, panel_scales, coord),
      GeomRect$draw_panel(box, panel_scales, coord),
      GeomSegment$draw_panel(crossbar, panel_scales, coord)
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "#FAFAFA", fill = "#51a7f9", size = 0.5,
    alpha = 1, shape = 19, width=0.3),

  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)
