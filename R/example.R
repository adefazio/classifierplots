
example_classifierplots <- function() {
 classifierplots(example_predictions$test.y, example_predictions$pred.prob) 
}

example_classifierplots_folder <- function() {
 classifierplots_folder(example_predictions$test.y, example_predictions$pred.prob, "example_output") 
}

gen_example <- function() {
  if (!requireNamespace("LiblineaR", quietly = TRUE)) {
    stop("LiblineaR needed for this function to work. Please install it.",
      call. = FALSE)
  }
  if (!requireNamespace("SVMMaj", quietly = TRUE)) {
    stop("SVMMaj needed for this function to work. Please install it.",
      call. = FALSE)
  }
  
  X <- as.data.table(AusCredit$X) 
  #y <- AusCredit$y
  y <- factor(AusCredit$y, labels=c(0, 1), levels=c("Rejected", "Accepted"))

  # Liblinear requires a matrix datatype. We all remove the bias with 0+.
  # If y was part of the data table, we would use "y ~ 0 + ." instead
  X.mm <- model.matrix(~ 0 + ., data=X)

  # Here I pull out just train/test sets. Be sure to pull out a val set
  # as well if your tuning hyperparameters.
  smpl_frac <- 0.5
  #seed(42)
  #train.ind 	<- sample.int(nrow(X), smpl_frac*nrow(X))
  train.ind <- c(1:345)
  train.mm 	<- X.mm[train.ind,]
  test.mm 	<- X.mm[-train.ind,]
  train.data <- X[train.ind,]
  test.data <- X[-train.ind,]
  train.y <- y[train.ind]
  test.y <- y[-train.ind]

  # Defaults are pretty reasonable.
  fit.ll <- LiblineaR(data=train.mm, target=train.y, type=0, cost=1, epsilon=0.0001, verbose=T)

  pred.ll 	<- predict(fit.ll, test.mm, proba=T)
  pred.prob <- pred.ll$probabilities[,"1"]

  test.y <- as.numeric(test.y) - 1 
  values <- train.mm[,"X2"]
  
  #classifierplots(test.y, pred.prob) 
  example_predictions <- list(test.y=test.y, pred.prob=pred.prob)
  devtools::use_data(example_predictions)
}
