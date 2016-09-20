#' @title calibration_plot
#' @description Returns a ggplot2 plot object containing a smoothed propensity @@ prediction level plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @export
calibration_plot <- function(test.y, pred.prob) {
  nbuckets = 10
  bucket_array <- seq(0.0, 1.0, by=0.1)
  positive_in_band <- function(bucket) {
    in_bucket_indicator <- pred.prob >= bucket_array[bucket] & pred.prob < bucket_array[bucket+1]
    bucket_size <- sum(in_bucket_indicator)
    positive <- sum(test.y[in_bucket_indicator] == 1)
    return(qbeta(c(llb=0.025, lb=0.25, y=0.5, ub=0.75, uub=0.965), 0.5+positive, 0.5+bucket_size-positive))
  }
  tbl <- data.table(bucket = 1:nbuckets, percentage = 5+bucket_array[1:nbuckets]*100, 
  blb=bucket_array[1:nbuckets], bub=bucket_array[(1:nbuckets) + 1])
  tbl <- cbind(tbl, 100*t(sapply(lift_tbl$bucket, positive_in_band)))

  ggplot(tbl, aes(x=percentage, y=y)) + 
    geom_ribbon(aes(ymin=llb, ymax=uub), fill=green_str, alpha="0.2") + 
  geom_ribbon(aes(ymin=lb, ymax=ub), fill=green_str, alpha="0.4") + 
  	geom_abline(slope=1.0, intercept=0, linetype="dotted") + 
  	scale_x_continuous(name="Predicted probability (%)", limits=c(0,100.0), breaks=seq(5, 95.0, 10.0)) + 
  	scale_y_continuous(name="Smoothed true probability (%)", limits=c(0,100.0)) + 
  	ggtitle("Calibration")
}

calibration_rolling_window <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  pred.order <- order(pred.prob,  decreasing=T)
  
  #  We choose the window size based on the amount of data, heuristically.
  if(length(test.y) > 2000) {
    window_split <- 20.0
  } else {
    window_split <- 10.0
  }
  
  window_radius <- abs(thesh_steps[2] - thesh_steps[length(thesh_steps)-1])/window_split
  #print(paste("Window radius: ", window_radius))
  
  propensity_tbl_perc <- data.table(
    part=1:length(step_array), percentage=100*step_array, 
    threshold=thesh_steps, step_array=step_array)
  propensity_tbl_perc[, propensity := 
    propensity_at_prediction_level(test.y, pred.prob, threshold, window_radius), by=c("threshold")]
  
  
  
  
  extreme_percentile <- quantile(pred.prob, 0.975)
  upper_prop <- propensity_at_prediction_level(test.y, pred.prob, extreme_percentile, window_radius)
  range_max <- max(extreme_percentile, upper_prop)
  
  return(ggplot(propensity_tbl_perc, aes(x=100*threshold, y=100.0*propensity)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours + 
    geom_abline(slope=1.0, intercept=0, linetype="dotted") + 
    scale_x_continuous(name="Predicted probability (%)", limits=c(0,100.0*range_max)) + 
    scale_y_continuous(name="True probability (%)", limits=c(0,100.0*range_max)) + 
    ggtitle("Calibration"))
}
