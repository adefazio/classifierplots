#' @title lift_plot
#' @description Returns a ggplot2 plot object containing an precision @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @param show_numbers Show numbers at deciles T/F default T.
#' @export
lift_plot <- function(test.y, pred.prob, granularity=0.02, show_numbers=T) {
  check_classifier_input_and_init(test.y, pred.prob)  
  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  tbl <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  tbl[, precision := sapply(threshold, function(x) precision_at_threshold(x, test.y, pred.prob))]
  tbl[, precision_lb := sapply(threshold, function(x) precision_at_threshold_p(0.025, x, test.y, pred.prob))]
  tbl[, precision_ub := sapply(threshold, function(x) precision_at_threshold_p(0.975, x, test.y, pred.prob))]
  
  baseline_rate <- tbl[percentage == 100, precision]
  
  tbl[, lift :=(precision-baseline_rate)/baseline_rate]
  tbl[, lift_lb :=(precision_lb-baseline_rate)/baseline_rate]
  tbl[, lift_ub :=(precision_ub-baseline_rate)/baseline_rate]

  if(show_numbers) {
    deciles <- seq(10, 100, 10)
    tbl[percentage %in% deciles, dec_lbl := paste0(format(100*lift, digits=2), "%")]
    numbers <- geom_text(aes(x=percentage, y=100*lift+2*lift, label=dec_lbl), 
              hjust=0.3, vjust=-1.0, size=4, color=I(blue_str))
  } else {
    numbers <- NULL
  }
  
  return(ggplot(tbl, aes(x=percentage, y=100*lift)) + 
    geom_ribbon(aes(ymin=100.0*lift_lb, ymax=100.0*lift_ub), fill=green_str, alpha="0.2") + 
    geom_abline(slope=0.0, intercept=1, linetype="dotted") + 
    geom_line(color=green_str, size=1.5) +  classifier_theme + classifier_colours +
    scale_x_continuous(name="k% (thresholded to positive class)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Relative lift (%)") + 
    numbers + 
    ggtitle("Lift"))
}
