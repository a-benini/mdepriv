test_that("mdepriv: arguments wa & wb", {
  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), output = "all")[-1],
    mdepriv(simul_data, c("y1", "y2"), wa = "cz", wb = "diagonal", output = "all")[-1]
  )

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), method = "ds", output = "all")[-1],
    mdepriv(simul_data, c("y1", "y2"), wa = "ds", wb = "diagonal", output = "all")[-1]
  )

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), method = "bv", output = "all")[-1],
    mdepriv(simul_data, c("y1", "y2"), wa = "bv", wb = "mixed", output = "all")[-1]
  )

  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), method = "equal", output = "all")[-1],
    mdepriv(simul_data, c("y1", "y2"), wa = "equal", wb = "diagonal", output = "all")[-1]
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), wa = "cz"),
    "The argument 'wa' is specified as \"cz\", whereas the argument 'wb' is unspecified. Possible options for 'wb' are: \"mixed\", \"pearson\" or \"diagonal\"."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), wb = "mixed"),
    "The argument 'wb' is specified as \"mixed\", whereas the argument 'wa' is unspecified. Possible options for 'wa' are: \"cz\", \"ds\", \"bv\" or \"equal\"."
  )

  expect_error(mdepriv(simul_data, c("y1", "y2"), wa = "cz", wb = "xxx"))

  expect_error(mdepriv(simul_data, c("y1", "y2"), wa = "xxx", wb = "mixed"))
})
