test_that("mdepriv: argument method", {
  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), output = "all"),
    mdepriv(simul_data, c("y1", "y2"), method = "cz", output = "all")
  )

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), method = "ds", output = "all"),
    mdepriv(simul_data, c("y1", "y2"), method = "ds", bv_corr_type = "pearson", rhoH = 0.3, output = "all")
  )

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), method = "bv", output = "all"),
    mdepriv(simul_data, c("y1", "y2"), method = "bv", bv_corr_type = "mixed", rhoH = NULL, output = "all")
  )

  expect_error(mdepriv(simul_data, c("y1", "y2"), method = "XXX"))

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"),
      method = "ds", bv_corr_type = "pearson", rhoH = 0.3,
      output = c(
        "weighting_scheme",
        "wa",
        "wb",
        "rhoH",
        "user_def_weights"
      )
    ),
    list(
      weighting_scheme = "Desai & Shah (1988) weighting scheme",
      wa = "ds",
      wb = "diagonal",
      rhoH = NA_real_,
      user_def_weights = NA
    )
  )
})
