test_that("mdepriv: argument output", {
  one_dim <- mdepriv(simul_data, c("y1", "y2"), output = "all")
  one_dim_summary_by_dimension <- mdepriv(simul_data, c("y1", "y2"), output = c("all", "summary_by_dimension"))
  two_dim <- mdepriv(simul_data, list(c("y1", "y2"), c("y3", "y4")), output = "all")

  expect_equal(setdiff(names(two_dim), names(one_dim)), "summary_by_dimension")

  expect_equal(setdiff(names(one_dim_summary_by_dimension), names(one_dim)), "summary_by_dimension")

  expect_equal(one_dim_summary_by_dimension[-c(3, 4)], one_dim[-3])

  expect_equal(names(two_dim), names(one_dim_summary_by_dimension))

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), output = list("xxx", "score_i")),
    "The argument 'output' is not of the class \"character\". It should be one or several of \"view\", \"all\", \"weighting_scheme\", \"aggregate_deprivation_level\", \"summary_by_dimension\", \"summary_by_item\", \"summary_scores\", \"score_i\", \"sum_sampling_weights\", \"data\", \"items\", \"sampling_weights\", \"wa\", \"wb\", \"rhoH\", \"user_def_weights\", \"score_i_heading\"."
  )

  expect_error(
    mdepriv(simul_data, c("y1", "y2"), output = c("xxx", "score_i")),
    "Invalid specification of the argument 'output': \"xxx\". The argument 'output' should be one or several of \"view\", \"all\", \"weighting_scheme\", \"aggregate_deprivation_level\", \"summary_by_dimension\", \"summary_by_item\", \"summary_scores\", \"score_i\", \"sum_sampling_weights\", \"data\", \"items\", \"sampling_weights\", \"wa\", \"wb\", \"rhoH\", \"user_def_weights\", \"score_i_heading\"."
  )
})
