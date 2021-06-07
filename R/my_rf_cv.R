#' random forest function
#'
#' This function performs random forest algorithm with k-fold cross validation.
#'
#' @param train input data frame
#' @param k_cv integer representing the number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error
#'
#' @examples
#' dat <- na.omit(my_penguins[, c("body_mass_g", "bill_length_mm",
#'                                "bill_depth_mm", "flipper_length_mm")])
#' my_rf_cv(dat, 2)
#'
#' @export
my_rf_cv <- function(train, k_cv) {
  # create a variable to randomly assigns observations to folds
  fold <- sample(rep(1 : k_cv, length = nrow(train)))
  # link the fold numbers to input data frame
  train["fold"] <- fold

  # creates a vector to store MSE's for each test fold
  MSE <- c(1 : k_cv)

  # iterate through each fold, and store predictions for each test fold
  for (i in 1 : k_cv) {
    # create a random forest model for predicting the test data with 100 trees
    model <- randomForest::randomForest(body_mass_g ~
                                          bill_length_mm +
                                          bill_depth_mm +
                                          flipper_length_mm,
                                        data = train[train$fold != i, ],
                                        ntree = 100)

    # calculate MSE for the test fold
    MSE[i] <- sum((train[train$fold == i, "body_mass_g"] -
                     predict(model, train[train$fold == i, -1])) ^ 2) /
      nrow(train[train$fold == i, ])
  }

  # get and return mean MSE
  return(mean(MSE))
}
