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

  # creates a data frame to store predictions for each element
  predictions <- data.frame("fold" = fold)

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
    # store predictions for the test data
    predictions[predictions$fold == i, "prediction"] <-
      predict(model, train[train$fold == i, -1])

    # calculate MSE for the test fold
    MSE[i] <- ((train[train$fold == i, "body_mass_g"] -
                  predictions[predictions$fold == i, "prediction"]) ^ 2) /
      sum(train$fold == i)
    MSE[i] <- mean(MSE[[i]])
  }

  # get and return mean MSE
  return(mean(unlist(MSE)))
}
