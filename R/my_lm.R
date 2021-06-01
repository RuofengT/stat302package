#' Linear Regression
#'
#' This function performs linear regression on given data
#'
#' @param formula Formula used for regression
#' @param data A dataset used for regression
#' @keywords inference prediction
#'
#' @return The estimate, the standard error, the t value, and Pr(>|t|) for
#' each coefficient including the intercept
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  # extract the model matrix
  mat <- model.matrix(formula, data)
  # extract a model frame object
  frame <- model.frame(formula, data)
  # extract the model response
  response <- model.response(frame)
  # solve for linear regression coefficients
  beta_hat <- solve(t(mat) %*% mat) %*% t(mat) %*% response
  # get degrees of freedom
  df <- length(mat[,1]) - length(mat[1,])

  # calculate the standard variance
  variance_hat <- 0
  for (i in 1 : length(mat[, 1])) {
    # sum over each observation
    variance_hat <- variance_hat + (response[i] - sum(mat[i, ] * beta_hat)) ^ 2
  }
  variance_hat <- variance_hat / df

  # calculate the standard error
  error_mat <- sqrt(diag(variance_hat * solve(t(mat) %*% mat)))

  # initialize vectors to store t values and p values
  t_value <- c(1:length(error_mat))
  p_value <- c(1:length(error_mat))

  # calculate t values and p values for each coefficient
  for (i in 1 : length(error_mat)) {
    t_value[i] <- (beta_hat[i, ]) / error_mat[i]
    p_value[i] <- pt(abs(t_value[i]), df, lower.tail = FALSE) * 2
  }

  # initialize an empty matrix
  results <- matrix(NA, length(error_mat), 4)
  # fill the matrix with required data: Estimations, standard errors,
  # t values, p values.
  results[, 1] <- beta_hat
  results[, 2] <- error_mat
  results[, 3] <- t_value
  results[, 4] <- p_value
  rownames(results) <- rownames(beta_hat)
  colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # return a table object
  return(as.table(results))
}
