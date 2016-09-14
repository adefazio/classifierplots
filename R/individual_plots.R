
check_predictions <- function(pred.prob) {
  
  if(max(pred.prob) > 1) {
      stop(paste("Pred.prob not in [0,1]. Max:", max(pred.prob)))
  } 
  
  if(min(pred.prob) < 0) {
      stop(paste("Pred.prob not in [0,1]. Min:", min(pred.prob)))
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
}


#' @title density.plot
#' @description Returns a ggplot2 plot object containing a score density plot.
#' If you have trouble displaying, it might be due to the font. Override by passing in something like "Sans" 
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @export
density.plot <- function(test.y, pred.prob) {
  check_classifier_input_and_init(test.y, pred.prob)  
  print("Generating score density plot")
  ground.truth <- factor(test.y)
  density_tbl <- data.table(Prediction=pred.prob, `Ground Truth`=ground.truth)
  
  mp <- max(quantile(pred.prob[ground.truth == 1], 0.95), 
            quantile(pred.prob[ground.truth != 1], 0.95))
  print(mp)
  
  if(mp < 0.4) {
    limits <- c(mp*1.1, 0.0)
  } else {
    limits <- c(1.0, 0.0)
  }
  
  annotation <- paste0("Test set size: ", length(test.y), 
    "\nNegative cases: ", format(100*sum(test.y != 1)/length(test.y), digits=3), 
    "%\nPositive cases:   ", format(100*sum(test.y == 1)/length(test.y), digits=3), "%")
  
  plt <- ggplot(density_tbl) + 
    geom_density(aes(x=Prediction, fill=`Ground Truth`), alpha=0.4, color=alpha('white', 0.0), size=1.5) + 
    scale_x_reverse(name="Probability threshold", limits=limits) + 
    scale_y_continuous(name="Density") + 
    ggtitle("Prediction density") + 
    annotation_custom(grob=grid::textGrob(annotation, x=0.05, y=0.87, just=c("left", "top"), 
      gp = grid::gpar(col=fontgrey_str))) + 
    legend_theme + classifier_theme + 
    theme(text=element_text(size=16, color="#444444")) + 
    classifier_colours
  return(plt)
}



#' @title accuracy.thresh.plot
#' @description Returns a ggplot2 plot object containing an accuracy @@ threshold plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
accuracy.thresh.plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  accuracy_tbl <- data.table(threshold=step_array)
  accuracy_tbl[, accuracy := sapply(threshold, function(x) accuracy_at_threshold(x, test.y, pred.prob))]
  
  print(paste0("Producing accuracy @ threshold plot"))
  return(ggplot(accuracy_tbl, aes(x=threshold, y=100.0*accuracy)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours + 
    scale_x_reverse(name="Probability threshold for positive prediction", limits=c(1.0,0)) + 
    scale_y_continuous(name="Accuracy (%)", limits=c(0,100)) + 
    ggtitle("Accuracy as a function of threshold"))
}

#' @title precision.thresh.plot
#' @description Returns a ggplot2 plot object containing an precision @@ threshold plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
precision.thresh.plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  print(paste0("Producing precision @ threshold plot"))
  step_array <- seq(0.0, 1.0, by=granularity)
  precision_tbl <- data.table(threshold=step_array)
  precision_tbl[, precision := sapply(threshold, function(x) precision_at_threshold(x, test.y, pred.prob))]
    
  return(ggplot(precision_tbl, aes(x=threshold, y=100.0*precision)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours +
    scale_x_reverse(name="Probability threshold for positive prediction", limits=c(1.0,0)) + 
    scale_y_continuous(name="Precision (% of positive predictions that are correct)", limits=c(0,100)) + 
    ggtitle("Precision-threshold"))
}

#' @title sensitivity.thresh.plot
#' @description Returns a ggplot2 plot object containing an sensitivity @@ threshold plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
sensitivity.thresh.plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  sensitivity_tbl <- data.table(threshold=step_array)
  sensitivity_tbl[, sensitivity := sapply(threshold, function(x) sensitivity_at_threshold(x, test.y, pred.prob))]

  return(ggplot(sensitivity_tbl, aes(x=threshold, y=100.0*sensitivity)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours +
    scale_x_reverse(name="Probability threshold for positive prediction", limits=c(1.0,0)) + 
    scale_y_continuous(name="Sensitivity (% of positive cases correctly predicted)", limits=c(0,100)) + 
    ggtitle("Sensitivity-threshold"))
}


#' @title accuracy.plot
#' @description Returns a ggplot2 plot object containing an accuracy @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
accuracy.plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  accuracy_tbl_perc <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  accuracy_tbl_perc[, accuracy := sapply(threshold, function(x) accuracy_at_threshold(x, test.y, pred.prob))]

  return(ggplot(accuracy_tbl_perc, aes(x=percentage, y=100.0*accuracy)) + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours + 
    scale_x_continuous(name="Recall (% thresholded to positive class)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Accuracy (%)", limits=c(0,100), breaks=seq(0.0, 100.0, 10.0)) + 
    ggtitle("Accuracy-Recall"))
}

#' @title propensity.plot
#' @description Returns a ggplot2 plot object containing an propensity @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
propensity.plot <- function(test.y, pred.prob, granularity=0.02) {
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
    scale_x_continuous(name="selection point (%)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Propensity of selected (%)") + 
    ggtitle("Positive percentage of selected"))
}

#' @title calibration.plot
#' @description Returns a ggplot2 plot object containing a smoothed propensity @@ prediction level plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
calibration.plot <- function(test.y, pred.prob, granularity=0.02) {
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
  print(paste("Window radius: ", window_radius))
  
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
    scale_x_continuous(name="Predicted propensity (%)", limits=c(0,100.0*range_max)) + 
    scale_y_continuous(name="Smoothed true propensity(%)", limits=c(0,100.0*range_max)) + 
    ggtitle("Calibration"))
}

#' @title precision.plot
#' @description Returns a ggplot2 plot object containing an precision @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @param show_numbers Show numbers at deciles T/F default T.
#' @export
precision.plot <- function(test.y, pred.prob, granularity=0.02, show_numbers=T) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  precision_tbl_perc <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  precision_tbl_perc[, precision := 100.0*sapply(threshold, function(x) precision_at_threshold(x, test.y, pred.prob))]

  if(show_numbers) {
    deciles <- seq(10, 100, 10)
    precision_tbl_perc[percentage %in% deciles, dec_lbl := paste0(format(precision, digits=2), "%")]
    numbers <- geom_text(aes(x=percentage, y=precision+0.02*precision, label=dec_lbl), 
              hjust=0.3, vjust=-1.0, size=4, color=I(blue_str))
  } else {
    numbers <- NULL
  }
  
  mp <- max(precision_tbl_perc[!is.na(precision),]$precision)
  
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
    geom_line(color=green_str, size=1.5) +  classifier_theme + classifier_colours +
    scale_x_continuous(name="Recall (% thesholded to positive class)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Precision (% of positive predictions that are correct)", limits=limits, breaks=breaks) + 
    numbers + 
    ggtitle("Precision-Recall"))
}

#' @title sensitivity.plot
#' @description Returns a ggplot2 plot object containing an sensitivity @@ percentile plot
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param granularity Default 0.02, probability step between points in plot.
#' @export
sensitivity.plot <- function(test.y, pred.prob, granularity=0.02) {
  check_classifier_input_and_init(test.y, pred.prob)  
  step_array <- seq(0.0, 1.0, by=granularity)
  thesh_steps <- round(quantile(pred.prob, step_array), digits=4)
  sensitivity_tbl_perc <- data.table(percentage=100 - 100*step_array, threshold=thesh_steps)
  sensitivity_tbl_perc[, sensitivity := sapply(threshold, function(x) sensitivity_at_threshold(x, test.y, pred.prob))]
  sensitivity_tbl_perc[, sensitivity_lb := sapply(threshold, function(x) sensitivity_at_threshold_p(0.025, x, test.y, pred.prob))]
  sensitivity_tbl_perc[, sensitivity_ub := sapply(threshold, function(x) sensitivity_at_threshold_p(0.975, x, test.y, pred.prob))]
  
  return(ggplot(sensitivity_tbl_perc, aes(x=percentage, y=100.0*sensitivity)) + 
    geom_ribbon(aes(ymin=100.0*sensitivity_lb, ymax=100.0*sensitivity_ub), fill=green_str, alpha="0.4") + 
    geom_line(color=green_str, size=1.5) + classifier_theme + classifier_colours +
    scale_x_continuous(name="Recall (% thesholded to positive class)") + 
    scale_y_continuous(name="Sensitivity (% of positive cases correctly predicted)", limits=c(0,100)) + 
    ggtitle("Sensitivity-Recall"))
}

#' @title roc.plot
#' @description A non-ugly ROC curve! Returns a ggplot2 plot object.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param font You can override the font here.
#' @param smooth Default F. Apply smoothing to the ROC curve. It's a little slow but looks good. Only works with bootstraping on.
#' @param use_bootstrap Default autodetect. It's really slow for more than say 1k test points, so be aware of this.
#' @export
roc.plot <- function(test.y, pred.prob, use_bootstrap=NULL, smooth=F) {
  require(pROC)
  check_classifier_input_and_init(test.y, pred.prob)  
  
  n <- length(test.y)
  rocr_pred <- ROCR::prediction(pred.prob, test.y)
    
  if(is.null(use_bootstrap)) {
    use_bootstrap <- n <= 10000
  }
  if(use_bootstrap && n > 100000) {
   stop(paste("Your trying to bootstrap on too large a dataset: ", n)) 
  }
    
  # No point in the ci calculation for large datasets
  big_data_cutoff <- 300000
  if(n < big_data_cutoff) {
    print("Forming pROC ROC object ...")
    roc <- pROC::roc(response=test.y, predictor=pred.prob, smooth=smooth, algorithm=3)
    #Uses: Elisabeth R. DeLong, David M. DeLong and Daniel L. Clarke-Pearson (1988)
    #  "Comparing the areas under two or more correlated 
    #  receiver operating characteristic curves: a nonparametric approach".
    civ <- pROC::ci(roc)*100.0
    auc <- civ[2]
    
    digits_use <- 3
    if(format(civ[1], digits=digits_use) == format(civ[3], digits=digits_use)) {
     digits_use <- 5 
    }
  } else {
    auc.perf <- ROCR::performance(rocr_pred,"auc"); 
    auc <- 100.0*as.numeric(auc.perf@y.values)
  }
  
  if(use_bootstrap) {
    specificities <- seq(0, 1, 0.05)
    print("BOOTSTRAPPING ROC curve (SLOW) ...")
    cse <- pROC::ci.se(roc, specificities=specificities, boot.stratified=T)

    perf_tbl = data.table(tpr=cse[, "50%"], fpr=1-specificities, ymin=cse[, "2.5%"], ymax=cse[, "97.5%"])
  } else {
    # roc method gives a non-increasing curve for some reason.
    #perf_tbl <- data.table(tpr=c(0, roc$sensitivities), fpr=c(0, 1-roc$specificities)) 
    # so we use ROCR package version
    print("Forming ROCR performance object")
    perf_s3 <- ROCR::performance(rocr_pred,'tpr','fpr')
    perf_tbl <- data.table(tpr=unlist(perf_s3@y.values), fpr=unlist(perf_s3@x.values))
  }
    
  print("Producing ROC plot")
  plt <- ggplot(perf_tbl, aes(x=100.0*fpr, y=100.0*tpr)) + 
     geom_line(color=green_str, size=1.5) + 
     geom_abline(slope=1.0, intercept=0, linetype="dotted") +
     annotate("text", x=62.5, y=37.5, 
         label=paste0("AUC ", format(auc, digits=3), "%"), 
         parse=F, size=7, colour=fontgrey_str) +
     scale_x_continuous(name="False Positive Rate (%)    (1-Specificity)", 
       limits=c(0.0, 100.0), expand=c(0, 0.3)) +
     scale_y_continuous(name="True Positive Rate (%)    (Sensitivity)", 
       limits=c(0.0, 100.0), expand=c(0, 0.3)) + 
    classifier_theme
    
  if(n < big_data_cutoff) {
      plt <- plt + annotate("text", x=62.5, y=30, 
          label=paste0("95% CI [", format(civ[1], digits=digits_use), ", ", format(civ[3], digits=digits_use), "]"), 
          parse=F, size=4.5, colour=fontgrey_str)
  }
     
  if(use_bootstrap) {
    plt <- plt + geom_ribbon(aes(ymin=100.0*ymin, ymax=100.0*ymax), fill=green_str, alpha="0.4")
  }
    
  return(plt)
}

lift.plot <- function(test.y, pred.prob) {
  check_classifier_input_and_init(test.y, pred.prob)  
  
  nbuckets = 10
  bucket_array <- seq(1.0, 0.0, by=-0.1)
  bucket_quantiles <- quantile(pred.prob, bucket_array)
  positive_in_bucket <- function(bucket) {
    in_bucket_indicator <- pred.prob < bucket_quantiles[bucket] & pred.prob >= bucket_quantiles[bucket+1]
    bucket_size <- sum(in_bucket_indicator)
    positive <- sum(test.y[in_bucket_indicator] == 1)
    return(positive/bucket_size)
  }
  lift_tbl <- data.table(bucket = 1:nbuckets, percentage = 100.0-bucket_array[1:nbuckets]*100)
  lift_tbl[, positive_perc := 100.0*sapply(bucket, positive_in_bucket)]

  return(ggplot(lift_tbl, aes(x=percentage, y=positive_perc)) + 
    geom_bar(fill=green_str, stat="identity", position="identity") + 
    classifier_theme + classifier_colours +
    scale_x_continuous(name="Decile (non-cumulative %)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Propensity (%)") + 
    ggtitle("Positive instances per decile"))
}
  

# Suppress some warnings
utils::suppressForeignCheck(c("Prediction", "Ground Truth", "accuracy", "threshold", "precision", "sensitivity", "percentage", "fpr", "tpr"))
