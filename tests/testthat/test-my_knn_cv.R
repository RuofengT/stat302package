test_that("my_knn_cv works", {
  dat <- na.omit(my_penguins[, c("species", "bill_length_mm", "bill_depth_mm",
                                 "flipper_length_mm", "body_mass_g")])
  cl <- dat$species
  dat <- dat[, c("bill_length_mm", "bill_depth_mm",
                 "flipper_length_mm", "body_mass_g")]
  expect_type(my_knn_cv(dat, cl, 1, 5), "list")
})
