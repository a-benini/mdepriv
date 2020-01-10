test_that("mdepriv: argument bv_corr_type", {
  expect_error(mdepriv(simul_data, c("y1", "y2"), bv_corr_type = "polyserial"))

  expect_equal(mdepriv(simul_data, c("y1", "y2"), bv_corr_type = "pearson", output = "all")[-1],
               mdepriv(simul_data, c("y1", "y2"), wa = "cz", wb = "diagonal", output = "all")[-1])

  expect_equal(mdepriv(simul_data, c("y1", "y2"), method = "bv", bv_corr_type = "pearson", output = "all")[-1],
               mdepriv(simul_data, c("y1", "y2"), wa = "bv", wb = "pearson", output = "all")[-1])
  })
