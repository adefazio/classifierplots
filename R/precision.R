#' @title precision_plot
#' @description Returns a ggplot2 plot object containing an precision @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @param show_numbers Show numbers at deciles T/F default T.
#' @export
precision_plot <- function(test.y, pred.prob, granularity=0.02, show_numbers=T) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  precision_tbl_perc <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  precision_tbl_perc[, precision := 100.0*sapply(threshold, function(x) precision_at_threshold(x, test.y, pred.prob))]
  precision_tbl_perc[, precision_lb := sapply(threshold, function(x) precision_at_threshold_p(0.025, x, test.y, pred.prob))]
  precision_tbl_perc[, precision_ub := sapply(threshold, function(x) precision_at_threshold_p(0.975, x, test.y, pred.prob))]
  

  if(show_numbers) {
    deciles <- seq(10, 100, 10)
    precision_tbl_perc[percentage %in% deciles, dec_lbl := paste0(format(precision, digits=2), "%")]
    numbers <- geom_text(aes(x=percentage, y=precision+0.02*precision, label=dec_lbl), 
              hjust=0.3, vjust=-1.0, size=4, color=I(blue_str))
  } else {
    numbers <- NULL
  }
  
  mp <- max(precision_tbl_perc[!is.na(precision),]$precision)
  minp <- min(precision_tbl_perc[!is.na(precision),]$precision)

  # Smart y breaks calculation
  if(mp <= 0.2) {
    breaks <- seq(0.0, 0.2, 0.01) 
  } else {
   if(mp <= 2) {
    breaks <- seq(0.0, 2.0, 0.1) 
   } else {
     if(mp <= 20) {
      breaks <- seq(0.0, 20.0, 1.0) 
     } else {
       breaks <- seq(0.0, 100.0, 10.0)
     }
   }
  }
  
  if(mp <= 20) {
    limits <- c(0, mp*1.1)
  } else {
    limits <- c(0,100)
  }
  
  return(ggplot(precision_tbl_perc, aes(x=percentage, y=precision)) + 
    geom_ribbon(aes(ymin=100.0*precision_lb, ymax=100.0*precision_ub), fill=green_str, alpha="0.2") + 
    geom_abline(slope=0.0, intercept=minp, linetype="dotted") + 
    geom_line(color=green_str, size=1.5) +  classifier_theme + classifier_colours +
    scale_x_continuous(name="k% (thresholded to positive class)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Precision (%)", limits=limits, breaks=breaks) + 
    numbers + 
    ggtitle("Precision @ k"))
}
