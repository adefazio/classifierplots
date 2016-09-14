
fontgrey_str <- "#444444"
green_str <- "#78b45a"
blue_str <- "#51a7f9"
grey_str <- "#7F7F7F"

#' @title classifier_theme
#' @description A useful theme for classifier diagnostic plots
classifier_theme <- theme()
#classifier_theme <- theme(panel.background = element_rect(fill = '#EEEEEE', color=NULL)) + 
#    theme(plot.title=element_text(vjust=1.2), 
#      axis.title.x=element_text(vjust=-0.2), 
#      axis.title.y=element_text(vjust=0.4),
#      panel.grid.minor=element_line(size=0.3, colour="#F2F2F2"),
#      panel.grid.major=element_line.5, colour="#FAFAFA"),
#      text=element_text(family="CM Sans", size=16, color=ambiata))

classifier_colours <- theme()

legend_theme <- theme(
  legend.position=c(0.5, 0.95), 
  legend.background=element_rect(fill=alpha('white', 0.0)),
  legend.direction="horizontal")
