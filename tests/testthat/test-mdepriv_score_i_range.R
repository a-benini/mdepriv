test_that("mdepriv: score_i range", {
  library(dplyr)
  library(stringr)
  # library(magrittr)
  # library(visdat)

  # names(MSNA_HC)
  items_level_1 <- list("water" = str_subset(names(MSNA_HC), "^water_"), "sanit" = str_subset(names(MSNA_HC), "^sanit_"))

  vars_required_complete <- c(
    items_level_1 %>% unlist() %>% unname(),
    str_subset(names(MSNA_HC), "^ls_"),
    "sampl_weights"
  )

  # replacing some values with NA (-> reason for below filtering)
  MSNA_HC_with_NA <- MSNA_HC
  for (i in seq_along(vars_required_complete)) {
    MSNA_HC_with_NA[i * 10 + (-9:20), vars_required_complete[i]] <- NA
  }

  # NA not allowed among items and sampling_weights:
  expect_error(
    mdepriv(MSNA_HC_with_NA, items_level_1, "sampl_weights",
            wa = "equal", wb = "mixed",
            score_i_heading = "ls_4_WASH", output = "all")
  )

  # vis_dat(MSNA_HC_with_NA, sort_type = FALSE)

  # filter out observations with NA in variables required complete:
  MSNA_HC_filtered <- MSNA_HC_with_NA %>% filter(complete.cases(.[vars_required_complete]))

  model_level_1 <- mdepriv(MSNA_HC_filtered, items_level_1, "sampl_weights",
                           wa = "equal", wb = "mixed",
                           score_i_heading = "ls_4_WASH", output = "all")

  # save the "data" returned by the lower level model
  MSNA_HC_filtered_2 <- model_level_1$data

  items_level_2 <- str_subset(names(MSNA_HC_filtered_2), "^ls_")

  # saving higher level model wont work ...
  expect_error(model_level_2 <- mdepriv(MSNA_HC_filtered_2, items_level_2, method = "bv", output = "all"))
  # ... because 2nd / higher level items fail the range check
  expect_error(mdepriv:::check_items_range_(data = MSNA_HC_filtered_2, items = items_level_2))

  # inspect range of score_i in mdepriv() 1st-level output (= ls_4_WASH)
  expect_false(any(MSNA_HC_filtered_2$ls_4_WASH < 0))
  expect_true(any(MSNA_HC_filtered_2$ls_4_WASH > 1))
  expect_equal(sum(MSNA_HC_filtered_2$ls_4_WASH > 1), 9)
  score_i_greater_1 <- MSNA_HC_filtered_2$ls_4_WASH[MSNA_HC_filtered_2$ls_4_WASH > 1]
  expect_true(all((score_i_greater_1 - 1) < 1e-15))

  # get / check internal Weight when ...
  # ... model_level_1 <- mdepriv(data = MSNA_HC_filtered, items = items_level_1, ...)
  Weight <- model_level_1$summary_by_item$Weight
  Weight <- Weight[Weight != 1] # without total
  expect_equal(sum(Weight), 1)

  # reconstruct score_i when ...
  # ... model_level_1 <- mdepriv(data = MSNA_HC_filtered, items = items_level_1, ...)
  data    <- MSNA_HC_filtered      # internal mdepriv(data = MSNA_HC_filtered, ...)
  items   <- unlist(items_level_1) # internal mdepriv(items = items_level_1, ...)
  score_i <- as.matrix(data[, items]) %*% Weight
  score_i <- as.vector(score_i)

  # check reconstructed score_i vs. corresponding mdepriv() output:
  expect_equal(score_i, MSNA_HC_filtered_2$ls_4_WASH)

  # trim score_i in mdepriv() output
  MSNA_HC_filtered_2$ls_4_WASH[MSNA_HC_filtered_2$ls_4_WASH > 1] <- 1
  expect_false(any(MSNA_HC_filtered_2$ls_4_WASH > 1))

  # with the trimmed item ls_4_WASH as input the 2nd-level mdepriv() run works:
  # model_level_2 <- mdepriv(MSNA_HC_filtered_2, items_level_2, method = "bv", output = "all")
})
