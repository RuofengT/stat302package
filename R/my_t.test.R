# this function takes a numeric vector of data x, a string specifying types of
# t test wanted, with choices "two.sided", "less" or "greater", and it takes
# a number mu indicating the null hypothesis.
# it conducts t test with given parameters, and returns a list including:
# the numeric test statistic, the degrees of freedom, the type of t test given,
# and the numeric p-value.
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
