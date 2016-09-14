
#########################################

propensity_at_threshold <- function(test.y, prob.y, part, pred.order, part_quantiles) {
  window_each_side <- 4
  #part_quantiles is an increasing sequence of quantiles
  part_lb <- max(1, part-window_each_side)
  part_ub <- min(part+1+window_each_side, length(part_quantiles))
  in_part_indicator <- (prob.y < part_quantiles[part_ub] & 
                       prob.y >= part_quantiles[part_lb])
                      
  part_orders <- pred.order[in_part_indicator]

 # Use a gaussian weighting function, scaled to fit the data window's order statistics.
  part_weights <- dnorm(part_orders, mean=mean(part_orders), sd=sd(part_orders))
  part_weights <- part_weights/sum(part_weights)
     
  # Standard weighted proportion equation
  rate_prop <- t(part_weights) %*% (test.y[in_part_indicator] == 1)
  #browser()
  return(rate_prop)
}

propensity_at_prediction_level <- function(test.y, prob.y, pred.level, window_radius) {
  #part_quantiles is an increasing sequence of quantiles
  in_part_indicator <- (prob.y < pred.level+window_radius & 
                       prob.y >= pred.level-window_radius)
                      
  prob.sub <- prob.y[in_part_indicator]

 # Use a gaussian weighting function, scaled to fit the data window's order statistics.
  pweights <- dnorm(prob.sub, mean=mean(prob.sub), sd=sd(prob.sub))
  pweights <- pweights/sum(pweights)
     
  # Standard weighted proportion equation
  rate_prop <- t(pweights) %*% (test.y[in_part_indicator] == 1)
  #browser()
  return(rate_prop)
}

accuracy_at_threshold <- function(threshold, test.y, prob.y) {
  test.y.bin <- test.y == 1
  pred.y.bin <- prob.y >= threshold
  correct = sum(pred.y.bin == test.y.bin)
  return(correct/length(test.y))
}

precision_at_threshold <- function(threshold, test.y, prob.y) {
  test.y.bin <- test.y == 1
  pred.y.bin <- prob.y >= threshold
  true_positives <- sum(pred.y.bin & test.y.bin)
  false_positives <- sum(pred.y.bin & (!test.y.bin))
  return(true_positives/(true_positives+false_positives))
}

sensitivity_at_threshold <- function(threshold, test.y, prob.y) {
  test.y.bin <- test.y == 1
  pred.y.bin <- prob.y >= threshold
  true_positives <- sum(pred.y.bin & test.y.bin)
  return(true_positives/sum(test.y.bin))
}

# Posterior quantiles of sensitivity
sensitivity_at_threshold_p <- function(p, threshold, test.y, prob.y) {
  test.y.bin <- test.y == 1
  pred.y.bin <- prob.y >= threshold
  true_positives <- sum(pred.y.bin & test.y.bin)
  return(qbeta(p, true_positives, sum(test.y.bin)-true_positives))
}

