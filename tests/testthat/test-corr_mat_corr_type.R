test_that("corr_mat: argument bv_corr_type", {
  expect_error(corr_mat(simul_data, c("y1", "y2"), corr_type = "polyserial"))
  expect_error(corr_mat(simul_data, c("y1", "y2"), corr_type = "diagonal"))
})
