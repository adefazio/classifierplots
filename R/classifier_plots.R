
#' @title classifier.plots
#' @description Produce a suit of classifier diagnostic plots, saving to disk.
#' @param test.y List of know labels on the test set
#' @param pred.prob List of probability predictions on the test set
#' @import data.table ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom ROCR performance
#' @importFrom ROCR prediction
#' @export
classifierplots <- function(test.y, pred.prob) {
  produce_classifier_plots(test.y, pred.prob, show=T)
}

#' @title classifier.plots
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

#' @importFrom gridExtra grid.arrange
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
    # Main ROCR object
    rocr_pred <- prediction(pred.prob, test.y)
  
    print("Calculating AUC")
    auc.perf <- performance(rocr_pred,"auc"); 
    auc <- as.numeric(auc.perf@y.values)
  
    print(paste("AUC is", auc)) 
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
  
  dens.plt <- density.plot(test.y, pred.prob)
  saveplot(dens.plt, "density.pdf")
  
  acc.plt <- accuracy.thresh.plot(test.y, pred.prob)
  saveplot(acc.plt, "accuracy.pdf") 
  
  prec.plt <- precision.thresh.plot(test.y, pred.prob)
  saveplot(prec.plt, "precision.pdf") 
    
  sen.plt <- sensitivity.thresh.plot(test.y, pred.prob)
  saveplot(sen.plt, "sensitivity.pdf")

  cal.plt <- calibration.plot(test.y, pred.prob)
  saveplot(cal.plt, "calibration.pdf")
  
  acc.plt.perc <- accuracy.plot(test.y, pred.prob)
  saveplot(acc.plt.perc, "percentage_accuracy.pdf")
  
  prec.plt.perc <- precision.plot(test.y, pred.prob)
  saveplot(prec.plt.perc, "percentage_precision.pdf")
  
  sen.plt.perc <- sensitivity.plot(test.y, pred.prob)
  saveplot(sen.plt.perc, "percentage_sensitivity.pdf")
  
  prop.plt.perc <- propensity.plot(test.y, pred.prob)
  saveplot(prop.plt.perc, "percentage_propensity.pdf")
  
  
  # Less ugly ROC curve
  roc.plt <- roc.plot(test.y, pred.prob)
  saveplot(roc.plt, "ROC.pdf")
  
  # lift plot
  lift.plt <- lift.plot(test.y, pred.prob)
  saveplot(lift.plt, "lift.pdf")
  
  if(!is.null(folder)) {
    print("Saving all plots in one file ...")
    all.plt.full.name <- paste0(folder, "/ALL.pdf")
    pdf(file=all.plt.full.name, width=25, height=13)
    gridExtra::grid.arrange(roc.plt, prop.plt.perc, lift.plt, cal.plt,
                 dens.plt, acc.plt.perc, prec.plt.perc, sen.plt.perc, ncol=4)
    dev.off()
    print(paste("Saved plot:", all.plt.full.name))
  }
  
  if(show) {
   dev.new.mine(width=25, height=13)
   return(gridExtra::grid.arrange(roc.plt, prop.plt.perc, lift.plt, cal.plt,
          dens.plt, acc.plt.perc, prec.plt.perc, sen.plt.perc, ncol=4)) 
  }
}

dev.new.mine <- function(width = 7, height = 7) { 
  platform <- sessionInfo()$platform 
  
  if (grepl("linux",platform)) { 
    x11(width=width, height=height)
  } else if (grepl("pc",platform)) { 
    windows(width=width, height=height) 
  } else if (grepl("apple", platform)) { 
    quartz(width=width, height=height, dpi=55) 
  } else {
    dev.new(width=width, height=height) 
  }
}

