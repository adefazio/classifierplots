
check_predictions <- function(pred.prob) {
  
  if(max(pred.prob) > 1) {
      stop(paste("Pred.prob not in [0,1]. Max:", max(pred.prob),
        ". You can use the sigmoid(x) function in this package to map to [0,1]."))
  } 
  
  if(min(pred.prob) < 0) {
      stop(paste("Pred.prob not in [0,1]. Min:", min(pred.prob),
        ". You can use the sigmoid(x) function in this package to map to [0,1]."))
  } 
}

check_classifier_input_and_init <- function(test.y, pred.prob) {

  if(length(test.y) != length(pred.prob)) {
    stop(paste("Length of test.y:", length(test.y), "did not match pred.prob:", length(pred.prob)))
  } 
  yvals <- unique(test.y)
  if(length(yvals) != 2) {
    stop(paste("test.y had more than 2 unique values:", length(yvals)))
  }
  if(sum(yvals == 1.0) != 1) {
    stop(paste("This code expects test.y to be numerical, with the positive class indicated by '1'. There was no 1 in test.y!"))
  }
  
  check_predictions(pred.prob)
}

#' @title sigmoid
#' @description Logistic sigmoid function, that maps any real number to the [0,1] interval. Supports vectors of numeric.
#' @param x data
#' @export
sigmoid <- function(x) { 1.0/(1.0+exp(-x)) }

#' @title propensity_plot
#' @description Returns a ggplot2 plot object containing an propensity @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
propensity_plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  pred.order <- order(pred.prob,  decreasing=T)
  
  propensity_tbl_perc <- data.table(
    part=1:length(step_array), percentage=100 - 100*step_array, 
    threshold=thesh_steps, step_array=step_array)
  propensity_tbl_perc[, propensity := 
    propensity_at_threshold(test.y, pred.prob, part, pred.order, thesh_steps), by=c("part")]
  
  return(ggplot(propensity_tbl_perc, aes(x=percentage, y=100.0*propensity)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours + 
    scale_x_continuous(name="Instance decile (non-cumulative %)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Smoothed positive (%)") + 
    ggtitle("Positive rate (rolling window)"))
}

#' @title accuracy_plot
#' @description Returns a ggplot2 plot object containing an accuracy @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @param show_numbers Show values as numbers above the plot line
#' @export
accuracy_plot <- function(test.y, pred.prob, granularity=0.02, show_numbers=T) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  accuracy_tbl_perc <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  accuracy_tbl_perc[, accuracy := sapply(threshold, function(x) accuracy_at_threshold(x, test.y, pred.prob))]
  accuracy_tbl_perc[, accuracy_lb := sapply(threshold, function(x) accuracy_at_threshold_p(0.025, x, test.y, pred.prob))]
  accuracy_tbl_perc[, accuracy_ub := sapply(threshold, function(x) accuracy_at_threshold_p(0.975, x, test.y, pred.prob))]
  
  if(show_numbers) {
    deciles <- seq(0, 100, 10)
    accuracy_tbl_perc[percentage %in% deciles, dec_lbl := paste0(format(100*accuracy, digits=2), "%")]
    numbers <- geom_text(aes(x=percentage, y=102*accuracy, label=dec_lbl), 
              hjust=0.3, vjust=-1.0, size=4, color=I(blue_str))
  } else {
    numbers <- NULL
  }

  return(ggplot(accuracy_tbl_perc, aes(x=percentage, y=100.0*accuracy)) + 
    geom_ribbon(aes(ymin=100.0*accuracy_lb, ymax=100.0*accuracy_ub), fill=green_str, alpha="0.2") + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours + 
    scale_x_continuous(name="k% (thresholded to positive class)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Accuracy (%)", limits=c(0,100), breaks=seq(0.0, 100.0, 10.0)) + 
    numbers + 
    ggtitle("Accuracy @ k"))
}

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

#' @title recall_plot
#' @description Returns a ggplot2 plot object containing an sensitivity @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @param show_numbers Show numbers at deciles T/F default T.
#' @export
recall_plot <- function(test.y, pred.prob, granularity=0.02, show_numbers=T) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  tbl <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  tbl[, sensitivity := sapply(threshold, function(x) sensitivity_at_threshold(x, test.y, pred.prob))]
  tbl[, sensitivity_lb := sapply(threshold, function(x) sensitivity_at_threshold_p(0.025, x, test.y, pred.prob))]
  tbl[, sensitivity_ub := sapply(threshold, function(x) sensitivity_at_threshold_p(0.975, x, test.y, pred.prob))]
  
  if(show_numbers) {
    deciles <- seq(10, 100, 10)
    tbl[percentage %in% deciles, dec_lbl := paste0(format(100*sensitivity, digits=2), "%")]
    numbers <- geom_text(aes(x=percentage, y=100*sensitivity+2*sensitivity, label=dec_lbl), 
              hjust=0.3, vjust=3.0, size=4, color=I(blue_str))
  } else {
    numbers <- NULL
  }
  
  return(ggplot(tbl, aes(x=percentage, y=100.0*sensitivity)) + 
    geom_ribbon(aes(ymin=100.0*sensitivity_lb, ymax=100.0*sensitivity_ub), fill=green_str, alpha="0.2") + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours +
    scale_x_continuous(name="k% (thresholded to positive class)", breaks=seq(0.0, 100.0, 10.0), limits=c(0,100), expand=c(0, 0.3)) + 
    scale_y_continuous(name="Recall (%)", breaks=seq(0.0, 100.0, 10.0), limits=c(0,100), expand=c(0, 0.3)) + 
    numbers + 
    ggtitle("Recall @ k"))
}

# Variables used in data.table expressions have to be defined here
utils::globalVariables(c(
  "Prediction", "Ground Truth", "accuracy", "threshold", 
  "precision", "sensitivity", "percentage", "fpr", "tpr",
  "propensity", "positive_perc", "bucket", "dec_lbl", "part",
  "ymin", "ymax", "sensitivity_lb", "sensitivity_ub"))
