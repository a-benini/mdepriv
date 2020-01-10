test_that("mdepriv: argument data", {
  expect_equal(corr_mat(simul_data, c("y1", "y2", "y5")),
               corr_mat(as.matrix(simul_data), c("y1", "y2", "y5")))

  expect_error(corr_mat("simul_data", c("y1", "y2", "y5")),
               "The argument 'data' is neither of the class \"data.frame\" nor of the class \"matrix\".")
  })
