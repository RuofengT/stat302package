#' k-nearest-neighbor function
#'
#' This function performs k nearest neighbors with k-fold cross validation.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords prediction
#'
#' @return a list with objects "class": a vector of the predicted class
#' for all observations; "cv_err": a numeric with the cross-validation
#' misclassification error
#'
#' @examples
#'
#' dat <- na.omit(my_penguins[, c("species", "bill_length_mm", "bill_depth_mm",
#'                                "flipper_length_mm", "body_mass_g")])
#' cl <- dat$species
#' dat <- dat[, c("bill_length_mm", "bill_depth_mm",
#'                "flipper_length_mm", "body_mass_g")]
#' my_knn_cv(dat, cl, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # create a variable to randomly assigns observations to folds
  fold <- sample(rep(1 : k_cv, length = nrow(train)))
  # link the fold numbers to input data frame
  train["fold"] <- fold
  # creates a vector to store MSE for each fold
  MSE <- c(1 : k_cv)

  # iterate through each fold, and calculate MSE for each test fold
  for (i in 1 : k_cv) {
    MSE[i] <- sum(class::knn(train[train$fold != i, ],
                             train[train$fold == i, ],
                             cl[train$fold != i],
                             k = k_nn) !=
                    cl[train$fold == i]) /
      nrow(train[train$fold == i, ])
  }

  # get misclassification rate
  cv_error <- mean(MSE)

  # get predicted classes for each observation
  class <- class::knn(train, train, cl, k_nn)

  # return a list containing both results
  return(list(class, cv_error))
}
