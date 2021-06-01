test_that("my_lm works mathematically", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)[1, 1],
               47.888516)
})
