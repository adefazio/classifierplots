#' @title density_plot
#' @description Returns a ggplot2 plot object containing a score density plot.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom stats dnorm
#' @importFrom stats qbeta
#' @importFrom stats quantile
#' @importFrom stats sd
#' @export
density_plot <- function(test.y, pred.prob) {
  check_classifier_input_and_init(test.y, pred.prob)  
  print("Generating score density plot")
  ground.truth <- factor(test.y)
  density_tbl <- data.table(Prediction=pred.prob, `Ground Truth`=ground.truth)
  
  mp <- max(quantile(pred.prob[ground.truth == 1], 0.95), 
            quantile(pred.prob[ground.truth != 1], 0.95))
  
  if(mp < 0.4) {
    limits <- c(mp*1.1, 0.0)
  } else {
    limits <- c(1.0, 0.0)
  }
  
  annotation <- paste0("Test set size: ", ifelse(length(test.y)==500000, ">= 500,000", length(test.y)), 
    "\nNegative cases: ", format(100*sum(test.y != 1)/length(test.y), digits=3), 
    "%\nPositive cases:   ", format(100*sum(test.y == 1)/length(test.y), digits=3), "%")
  
  plt <- ggplot(density_tbl) + 
    geom_density(aes(x=Prediction, fill=`Ground Truth`), alpha=0.4, , size=1.5) + 
    scale_x_reverse(name="Probability threshold", limits=limits) + 
    scale_y_continuous(name="Density", expand=c(0,0)) + 
    ggtitle("Prediction density") + 
    annotation_custom(grob=grid::textGrob(annotation, x=0.05, y=0.87, just=c("left", "top"), 
      gp = grid::gpar(col=fontgrey_str))) + 
    legend_theme + classifier_theme + 
    #theme(text=element_text(size=16, color="#444444")) + 
    classifier_colours
  return(plt)
}
