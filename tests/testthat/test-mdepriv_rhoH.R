test_that("mdepriv: argument rhoH", {
  expect_equal(mdepriv(simul_data, c("y1", "y2"), method = "bv"),
               mdepriv(simul_data, c("y1", "y2"), method = "bv", rhoH = NULL))

  expect_equal(mdepriv(simul_data, c("y1", "y2"), method = "bv"),
               mdepriv(simul_data, c("y1", "y2"), method = "bv", rhoH = NA))

  expect_equal(mdepriv(simul_data, c("y1", "y2"), method = "bv"),
               mdepriv(simul_data, c("y1", "y2"), method = "bv", rhoH = NA_real_))

  test_na <- mdepriv(simul_data, c("y1", "y2"), rhoH = 0.5, output = "rhoH")
  expect_equal(is.na(test_na) & is.numeric(test_na), TRUE)

  expect_equal(mdepriv(simul_data, c("y1", "y2"), method = "bv", rhoH = 0.5, output = "rhoH"),
               0.5)

  expect_equal(mdepriv(simul_data, c("y1", "y2", "y3", "y4", "y5", "y6", "y7"), method = "bv", output = "rhoH"),
               0.656301573242914)

  expect_error(mdepriv(simul_data, c("y1", "y2"), rhoH = "XXX"), "The argument 'rhoH' is not numeric.")

  expect_error(mdepriv(simul_data, c("y1", "y2"), rhoH = -2))
  })
