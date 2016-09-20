
#' @title positives_plot
#' @description Returns a ggplot2 plot object containing an positives-per-decile plot.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @export
positives_plot <- function(test.y, pred.prob) {
  check_classifier_input_and_init(test.y, pred.prob)  
  
  nbuckets = 10
  bucket_array <- seq(1.0, 0.0, by=-0.1)
  bucket_quantiles <- quantile(pred.prob, bucket_array)
  positive_in_bucket <- function(bucket) {
    in_bucket_indicator <- pred.prob < bucket_quantiles[bucket] & pred.prob >= bucket_quantiles[bucket+1]
    bucket_size <- sum(in_bucket_indicator)
    positive <- sum(test.y[in_bucket_indicator] == 1)
    return(qbeta(c(llb=0.025, lb=0.25, y=0.5, ub=0.75, uub=0.965), positive, bucket_size-positive))
  }
  tbl <- data.table(bucket = 1:nbuckets, percentage = 100.0-bucket_array[1:nbuckets]*100)
  tbl <- cbind(tbl, 100*t(sapply(tbl$bucket, positive_in_bucket)))

  return(ggplot(tbl, aes(x=percentage, y=y, ymin=llb, lower=lb, middle=y, upper=ub, ymax=uub)) + 
    geom_ambiboxplot(fill=green_str, stat="identity", position="identity", width=8) + 
    classifier_theme + classifier_colours +
    scale_x_continuous(name="Instance decile (non-cumulative %)", breaks=seq(0.0, 100.0, 10.0)) + 
    scale_y_continuous(name="Positive instances in decile (%)") +
    ggtitle("Positive instances per decile"))
}
  
