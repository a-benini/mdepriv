test_that("mdepriv: argument data", {
  expect_equal(
    mdepriv(simul_data, c("y1", "y2", "y5")),
    mdepriv(as.matrix(simul_data), c("y1", "y2", "y5"))
  )

  expect_error(
    mdepriv("simul_data", c("y1", "y2", "y5")),
    "The argument 'data' is neither of the class \"data.frame\" nor of the class \"matrix\"."
  )

  expect_equal(
    names(mdepriv(simul_data, c("y1", "y2", "y5"), output = "data")),
    c(names(simul_data), "score_i")
  )

  expect_equal(
    names(mdepriv(simul_data, c("y1", "y2", "y5"), score_i_heading = "score_i_user_name", output = "data")),
    c(names(simul_data), "score_i_user_name")
  )

  model <- mdepriv(simul_data, c("y1", "y2", "y3", "y4", "y5", "y6", "y7"), output = "all")

  expect_equal(sum(model$score_i), 37.1957592834387)

  expect_equal(model$score_i, model$data$score_i)

  expect_error(
    mdepriv(model$data, model$items),
    "\"score_i\" is not valid as argument 'score_i_heading' for the current model. For the argument 'data' already includes a column by this name, possibly as the result of a previous mdepriv model. Therefore, give a different name for the scores column in the output data by specifying the argument 'score_i_heading'."
  )
})
