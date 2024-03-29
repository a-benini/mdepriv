test_that("mdepriv: argument sampling_weights", {
  expect_equal(mdepriv(simul_data, c("y1", "y2"), "sampl_weights", output = "sampling_weights"), "sampl_weights")
  expect_equal(mdepriv(simul_data, c("y1", "y2"), output = "sampling_weights"), NA_character_)
  expect_equal(
    mdepriv(simul_data, c("y1", "y2"), output = "all"),
    mdepriv(simul_data, c("y1", "y2"), sampling_weights = NULL, output = "all")
    )
  expect_equal(mdepriv(simul_data, c("y1", "y2"), "sampl_weights", output = "sum_sampling_weights"), 100.237)
  expect_equal(mdepriv(simul_data, c("y1", "y2"), output = "sum_sampling_weights"), NA_real_)

  simul_data_2 <- simul_data
  simul_data_2$sampl_weights <- -simul_data_2$sampl_weights
  expect_error(
    mdepriv(simul_data_2, c("y1", "y2"), "sampl_weights"),
    "The argument 'sampling_weights', which is specified as variable \"sampl_weights\" and included in the argument 'data' contains values =< 0. Only positive values are valid as sampling weights."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), "y1"),
    "The argument 'sampling_weights', which is specified as variable \"y1\" is already among the variables selected with the argument 'items'."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), c(T, F)),
    "The argument 'sampling_weights' is not of the class \"character\". The argument 'sampling_weights' accepts only a single character string specifying a numeric variable within the argument 'data'."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), c("A", "B")),
    "The argument 'sampling_weights' is a character vector with a length of 2. The argument 'sampling_weights' accepts only a single character string specifying a numeric variable within the argument 'data'."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), "XXX"),
    "The argument 'sampling_weights' is a character string, as required; however, it does not match any variable within the argument 'data'."
  )

  simul_data_2 <- simul_data
  simul_data_2$sampl_weights[1] <- NA
  expect_error(
    mdepriv(simul_data_2, c("y1", "y2"), "sampl_weights"),
    "The argument 'sampling_weights', which is specified as variable \"sampl_weights\" and included in the argument 'data', is a numeric vector including NA-values. NA is not valid as sampling weight."
  )

  simul_data_2 <- simul_data
  simul_data_2$sampl_weights <- as.character(simul_data_2$sampl_weights)
  expect_error(
    mdepriv(simul_data_2, c("y1", "y2"), "sampl_weights"),
    "The argument 'sampling_weights', which is specified as variable \"sampl_weights\" and included in the argument 'data', is not a numeric vector."
  )
})
