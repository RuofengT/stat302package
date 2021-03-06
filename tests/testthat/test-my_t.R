test_that("my_t.test works mathematically", {
  expect_equal(as.numeric(my_t.test(my_gapminder$lifeExp, "two.sided", 60)[4]),
               0.09322877)
  expect_equal(as.numeric(my_t.test(my_gapminder$lifeExp, "less", 60)[4]),
               0.046614385)
  expect_equal(as.numeric(my_t.test(my_gapminder$lifeExp, "greater", 60)[4]),
               0.95338562)
})

test_that("my_t.test stops if test type is incorrect", {
  expect_error(my_t.test(my_gapminder$lifeExp, "what", 60),
                 'wrong input. Try "two.sided", "less", or "greater".')
})
