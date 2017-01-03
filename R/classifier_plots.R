
#' The main functions you want are \code{\link{classifierplots}} or \code{\link{classifierplots_folder}}.
#' @docType package
#' @name classifierplots
NULL

#' \figure{example.png}
#' @title classifierplots
#' @description Produce a suit of classifier diagnostic plots
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @import data.table ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom ROCR performance
#' @importFrom ROCR prediction
#' @export
#' @examples
#' \dontrun{
#'  classifierplots(example_predictions$test.y, example_predictions$pred.prob)
#' }
classifierplots <- function(test.y, pred.prob) {
  produce_classifier_plots(test.y, pred.prob, show=T)
}

#' @title classifierplots_folder
#' @description Produce a suit of classifier diagnostic plots, saving to disk.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @param folder Directory to save plots into
#' @param height height of separately saved plots
#' @param width width of separately saved plots
#' @export
classifierplots_folder <- function(test.y, pred.prob, folder, height=5, width=5) {
  produce_classifier_plots(test.y, pred.prob, folder=folder, height=height, width=width, show=F)
}

#' @importFrom grid grid.draw
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices dev.new
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom grDevices quartz
#' @importFrom grDevices x11
#' @importFrom utils sessionInfo
produce_classifier_plots <- function(
  test.y, pred.prob,
  folder=NULL,
  height=5, width=5, show=F) {
  check_classifier_input_and_init(test.y, pred.prob)
  n <- length(test.y)

  if(!is.null(folder)) {
    dir.create(folder, showWarnings = F)
  }

  # Subsample data if it is huge
  if(n > 500000) {
    sel.ind <- sample.int(n, 500000)

    test.y <- test.y[sel.ind]
    pred.prob <- pred.prob[sel.ind]
  }

  if(!is.null(folder)) {
    auc <- calculate_auc(test.y, pred.prob)
    write(auc, file=paste0(folder, "/auc.txt"))
  }

  saveplot <- function(plt, plt.name, width=7, height=7) {
    if(!is.null(folder)) {
      full.plt.name <- paste0(folder, "/", plt.name)
      ggsave(plot=plt, filename=full.plt.name, width = width, height = height)
      print(paste("Saved plot:", full.plt.name))
    }
    invisible()
  }

  roc.plt <- roc_plot(test.y, pred.prob)
  saveplot(roc.plt, "ROC.pdf")

  positives.plt <- positives_plot(test.y, pred.prob)
  saveplot(positives.plt, "positives.pdf")

  cal.plt <- calibration_plot(test.y, pred.prob)
  saveplot(cal.plt, "calibration.pdf")

  notat.plt <- notation_key_plot()

  dens.plt <- density_plot(test.y, pred.prob)
  saveplot(dens.plt, "density.pdf")

  acc.plt.perc <- accuracy_plot(test.y, pred.prob)
  saveplot(acc.plt.perc, "accuracy.pdf")

  prec.plt.perc <- precision_plot(test.y, pred.prob)
  saveplot(prec.plt.perc, "precision.pdf")

  lift.plt <- lift_plot(test.y, pred.prob)
  saveplot(lift.plt, "lift.pdf")

  recall.plt <- recall_plot(test.y, pred.prob)
  saveplot(recall.plt, "recall.pdf")


  if(!is.null(folder)) {
    print("Saving all plots in one file ...")
    all.plt.full.name <- paste0(folder, "/ALL.pdf")
    g <- gridExtra::arrangeGrob(roc.plt, positives.plt, cal.plt, notat.plt,
                     dens.plt, acc.plt.perc, prec.plt.perc, recall.plt, ncol=4)

    ggsave(file=all.plt.full.name, g, width=25, height=13)
    print(paste("Saved plot:", all.plt.full.name))
  }

  if(show) {
   dev.new(width=25, height=13, dpi=55)
   #return(gridExtra::grid.arrange(roc.plt, positives.plt, cal.plt, notat.plt,
   #        dens.plt, acc.plt.perc, prec.plt.perc, recall.plt, ncol=4))
    g <- gridExtra::arrangeGrob(roc.plt, positives.plt, cal.plt, notat.plt,
                    dens.plt, acc.plt.perc, prec.plt.perc, recall.plt, ncol=4)
    grid::grid.draw(g)
    return(g)
  } else {
    invisible()
  }
}
