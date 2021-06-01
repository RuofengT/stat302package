#' T Test
#'
#' This function performs t test on given data
#'
#' @param x Numeric vector of data
#' @param alternative A string specifying types of t test wanted, with choices
#' "two.sided", "less" or "greater".
#' @param mu a number indicating the null hypothesis.
#' @keywords inference
#'
#' @return a list including: the numeric test statistic, the degrees of freedom,
#' the type of t test given, and the numeric p-value.
#'
#' @examples
#' my_t.test(my_gapminder$lifeExp, "two.sided", 60)
#' my_t.test(my_gapminder$lifeExp, "less", 60)
#' my_t.test(my_gapminder$lifeExp, "greater", 60)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # check if the type of t test wanted was legitimate. If the input is not from
  # choices provided, an error message will show up
  if (alternative != "two.sided" & alternative != "less" &
      alternative != "greater") {
    stop('wrong input. Try "two.sided", "less", or "greater".')
  }
  # calculate the observed mean value of input data
  mu_hat <- mean(x)
  # calculate the number of observations
  x_size <- length(x)
  # calculate the standard error of input data
  x_error <- sd(x) / sqrt(x_size)
  # calculate the test statistic
  t_stat <- (mu_hat - mu) / x_error
  # calculate p-value depending on types of t test wantded
  if (alternative == "two.sided") {
    t_prob <- pt(abs(t_stat), df = x_size - 1, lower.tail = FALSE) * 2
  }
  if (alternative == "less") {
    t_prob <- pt(t_stat, df = x_size - 1, lower.tail = TRUE)
  }
  if (alternative == "greater") {
    t_prob <- pt(t_stat, df = x_size - 1, lower.tail = FALSE)
  }
  # store results in a list, and name them properly
  result <- list(t_stat, x_size - 1, alternative, t_prob)
  names(result) <- (c("test statistic", "degree of freedom",
                      "type of t test", "p value"))
  # return the results as a list object
  return(result)
}
