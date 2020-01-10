test_that("mdepriv: argument items", {
  expect_equal(class(mdepriv(simul_data, c("y1", "y2", "y3"), output = "items")), "list")

  expect_error(mdepriv(simul_data, list('group A' = c("y1", "y2", "y3"), 'group A' = c("y4", "y5"))),
               "The argument 'items' contains duplicates among the labelled dimension names. If labelled, dimensions names must be unique."
               )

  expect_error(mdepriv(simul_data, list('group A' = c("y1", "y2", "y3"), c("y4", "y5"))),
               "The argument 'items' has been labelled with dimension names, but incompletely. Either all dimensions or strictly none must be labelled."
               )

  expect_error(mdepriv(simul_data, 1:4),
               "The argument 'items' is neither a vector nor a list of vectors constisting fully of character elements."
               )

  expect_error(mdepriv(simul_data, "y1"),
               "The argument 'items' has a length of 1. The argument 'items' has to include at least 2 variables contained in the argument 'data'.")

  expect_error(mdepriv(simul_data, c("y1", "y2", "y2")),
               "The argument 'items' contains repeatedly the following variable: \"y2\".")

  expect_error(mdepriv(simul_data, c("y1", "y1", "y2", "y2")),
               "The argument 'items' contains repeatedly the following variables: \"y1\" and \"y2\"."
               )

  expect_error(mdepriv(simul_data, c("y1", "y2", "X")),
               "The argument 'data' does not contain the following variable: \"X\". The argument 'items' may only include variables that are part of the argument 'data'."
               )

  expect_error(mdepriv(simul_data, c("y1", "Z", "X")),
               "The argument 'data' does not contain the following variables: \"Z\" and \"X\". The argument 'items' may only include variables that are part of the argument 'data'."
               )

  expect_error(mdepriv(simul_data, c("y1", "y2", "id")))
  expect_error(mdepriv(simul_data, c("y1", "y2", "id", "sampl_weights")))

  simul_data_2 <- simul_data
  simul_data_2[1, c("y1", "y2")] <- NA
  expect_error(mdepriv(simul_data_2, c("y1", "y3", "y4")),
               "The argument 'items' contains the following variable with NA: \"y1\". The argument 'items' does not allow any NA-values.")
  expect_error(mdepriv(simul_data_2, c("y1", "y2", "y3")),
               "The argument 'items' contains the following variables with NA: \"y1\" and \"y2\". The argument 'items' does not allow any NA-values.")

  simul_data_2$A <- "A"
  simul_data_2$B <- factor("B")
  expect_error(mdepriv(simul_data_2, c("y1", "y2", "A")),
               "The argument 'items' contains the following non-numeric variable: \"A\". The argument 'items' only allows numeric variables contained in the argument 'data'.")
  expect_error(mdepriv(simul_data_2, c("y1", "y2", "A", "B")),
               "The argument 'items' contains the following non-numeric variables: \"A\" and \"B\". The argument 'items' only allows numeric variables contained in the argument 'data'.")
  })
