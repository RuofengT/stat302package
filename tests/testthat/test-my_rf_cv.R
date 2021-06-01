test_that("my_rf_cv works", {
  dat <- na.omit(my_penguins[, c("body_mass_g", "bill_length_mm",
                                 "bill_depth_mm", "flipper_length_mm")])
  expect_type(my_rf_cv(dat, 2), "double")
})
