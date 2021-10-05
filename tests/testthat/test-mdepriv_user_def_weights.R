test_that("mdepriv: argument user_def_weights", {
  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), user_def_weights = c(0.25, 0.75), output = "user_def_weights"),
    list("Dimension 1" = c(0.25, 0.75))
  )
  test_na <- mdepriv(simul_data, c("y1", "y2"), output = "user_def_weights")
  expect_equal(is.na(test_na), TRUE)

  expect_equal(
    mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4")), output = "all"),
    mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4")), user_def_weights = NULL, output = "all")
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), user_def_weights = 1),
    "The number of elements per dimension in the argument 'user_def_weights' does not match the number of elements per dimension in the argument 'items'."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), user_def_weights = c(NA, 0.5)),
    "At least for one of the items the argument 'user_def_weights' is specified with a NA-value. If specified, the argument 'user_def_weights' has be numeric and defined for each item."
  )

  expect_error(
    mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4"), c("y5", "y6", "y7")),
      user_def_weights = list(c("a", "b"), c(0.25, 0.75), c(0.25, 0.5, 0.25))
    ),
    "The argument 'user_def_weights' is neither a vector nor a list of vectors consisting entirely of numeric elements."
  )

  expect_error(
    mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4"), c("y5", "y6", "y7")),
      user_def_weights = list(c(0.25, 0.75), c(0.25, 0.75), c(0.25, 0.5, 0.25), c(0.5, 0.5))
    ),
    "The number of dimension in the argument 'user_def_weights' does not match the number of dimensions specified in the argument 'items'."
  )

  expect_error(
    mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4"), c("y5", "y6", "y7")),
      user_def_weights = list(c(1, 1), c(0.25, 0.75), c(0.25, 0.5, 0.25))
    ),
    "At least for one dimension in the argument 'user_def_weights' the elements do not sum up to 1."
  )

  expect_error(
    mdepriv(simul_data, list("group A" = c("y1", "y2"), "group B" = c("y3", "y4"), "group C" = c("y5", "y6", "y7")),
      user_def_weights = list("group A" = c(0.25, 0.75), "group B" = c(0.25, 0.75), "group D" = c(0.25, 0.5, 0.25))
    ),
    "The labelling of the dimensions in the argument 'user_def_weights' does not match the labelling of the dimensions in the argument 'items'."
  )
})
