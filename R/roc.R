

utils::globalVariables(c(
  "accuracy_lb", "accuracy_ub", "preds", "y", "llb", "uub", "lb", "ub",
  "precision_lb", "precision_ub", "lift", "lift_ub", "lift_lb", "tp",
  "resample", "fp", "fpr_step", "50%", "2.5%", "97.5%"))

#' @title calculate_auc
#' @description Compute auc from predictions and truth
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @return auc
calculate_auc <- function(test.y, pred.prob) {
  n <- length(test.y)

  print("(AUC) Sorting data ...")
  test.y.bin <- test.y == 1
	roc_tbl <- data.table(y=test.y.bin, preds=pred.prob)
  roc_tbl <- roc_tbl[order(preds)]

	npositives <- as.double(sum(test.y.bin))
	nnegatives <- as.double(n - npositives)

  print("(AUC) Calculating ranks ...")
  # Main AUC calcuation. We use the MW-U stat equivalence,
  # since it's a little faster to calculate.
  # Note that tied predictions are given a rank equal to the mean of the tied set.
	roc_tbl[, rank := mean(.I), by=preds]

	r1 <- roc_tbl[y == T, sum(rank)]
	u1 <- r1 - (npositives*(npositives+1))/2.0
	auc <- 100*u1/(npositives*nnegatives)
  return(auc)
}

#' @title roc_plot
#' @description Produces a smoothed ROC curve as a ggplot2 plot object. A confidence interval is produced using bootstrapping, although it is turned off by default if you have a large dataset.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param resamps How many bootstrap samples to use
#' @param force_bootstrap True/False to force or force off bootstrapping.
#' @export
#' @importFrom caret createResample
roc_plot <- function(test.y, pred.prob, resamps=2000, force_bootstrap=NULL) {
  #check_classifier_input_and_init(test.y, pred.prob)

  n <- length(test.y)
	test.y.bin <- test.y == 1
	nbins <- 50

	npositives <- sum(test.y.bin)
	nnegatives <- n - npositives

	negative_steps <- floor(nnegatives/50.0)
  negative_steps <- floor(nnegatives/nbins)

  print("Calculating AUC ...")
  auc <- calculate_auc(test.y, pred.prob)
  print(paste("AUC:", auc))

  # No point in the ci calculation for large datasets
  big_data_cutoff <- 50000
  if(!is.null(force_bootstrap)) {
    bootstrap <- force_bootstrap
  } else {
    bootstrap <- n <= big_data_cutoff
  }

  # Negated to get the correct sort order later
  pos_pred_probs <- -pred.prob[test.y.bin]
  neg_pred_probs <- -pred.prob[!test.y.bin]

  if(bootstrap) {
    print("Bootstrapping ROC curves")

  	pos_pred_boots <- pos_pred_probs[c(caret::createResample(pos_pred_probs, times=resamps, list=F))]
  	neg_pred_boots <- neg_pred_probs[c(caret::createResample(neg_pred_probs, times=resamps, list=F))]

  	roc_tbl <- data.table(
  		preds=c(pos_pred_boots, neg_pred_boots),
  		y=c(rep(T, length(pos_pred_boots)),  rep(F, length(neg_pred_boots))),
  		resample=c(rep(1:resamps, each=length(pos_pred_probs)),
  				   rep(1:resamps, each=length(neg_pred_probs))))
  	setkey(roc_tbl, "resample", "preds")

  	roc_tbl[, tp := cumsum(y), by=resample]
  	roc_tbl[, fp := cumsum(!y), by=resample]
  	roc_tbl[, fpr_step := ((fp %% negative_steps) == 0), by=resample]

  	substeps_tbl <- roc_tbl[fpr_step == T, ]
    # there can be multiple rows with the same fpr, so we pick the last.
  	subind <- substeps_tbl[, .I[.N], by = c("resample", "fp")]
  	roc_tbl_sub <- substeps_tbl[subind$V1]
  	roc_tbl_sub_stats <- roc_tbl_sub[, as.list(quantile(tp, c(0.025, 0.5, 0.975))), keyby=fp]

    print("Eval AUC")
  	roc_tbl[, rank := mean(.I), by = c("resample", "preds")]

  	r1 <- roc_tbl[y == T, sum(rank) - .N*n*(resample-1), keyby="resample"]$V1
  	u1 <- r1 - (npositives*(npositives+1))/2.0
  	aucs <- 1.0 - u1/(npositives*nnegatives)

  	#auc_tbl_ex <- roc_tbl[roc_tbl[, .I[.N], by = c("resample", "fp")]$V1]
    #aucs <- auc_tbl_ex[, sum(tp)/(npositives*nnegatives), by=resample]$V1
    auc_bounds <- 100.0*quantile(aucs, c(0.025, 0.5, 0.975))

    # Make sure we print enough digits so that the lower and upper bounds are not the same
    digits_use <- 3
    if(format(auc_bounds[1], digits=digits_use) == format(auc_bounds[3], digits=digits_use)) {
     digits_use <- 5
    }
  } else {
    roc_tbl <- data.table(
      preds=c(pos_pred_probs, neg_pred_probs),
      y=c(rep(T, length(pos_pred_probs)),  rep(F, length(neg_pred_probs))))
    setkey(roc_tbl, "preds")

    roc_tbl[, tp := cumsum(y)]
    roc_tbl[, fp := cumsum(!y)]
    roc_tbl[, fpr_step := ((fp %% negative_steps) == 0)]

    substeps_tbl <- roc_tbl[fpr_step == T, ]
    # there can be multiple rows with the same fpr, so we pick the last.
    subind <- substeps_tbl[, .I[.N], by = c("fp")]
    roc_tbl_sub_stats <- substeps_tbl[subind$V1]
    roc_tbl_sub_stats[, `50%` := tp]
  }

  print("Producing ROC plot")

  plt <- ggplot(roc_tbl_sub_stats, aes(
      x=100.0*fp/nnegatives,
      y=100.0*`50%`/npositives)) +
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

  if(bootstrap) {
      plt <- plt +
        geom_ribbon(aes(
            ymin=100.0*`2.5%`/npositives,
            ymax=100.0*`97.5%`/npositives),
            fill=green_str, alpha="0.2") +
        annotate("text", x=62.5, y=30,
          label=paste0("95% CI: ", format(auc_bounds[1], digits=digits_use), "% - ", format(auc_bounds[3], digits=digits_use), "%"),
          parse=F, size=4.5, colour=fontgrey_str)
  }

  return(plt + ggtitle("ROC"))
}
