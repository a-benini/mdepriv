test_that("corr_mat: argument output", {
  items_sel <- c("y1", "y4", "y5", "y6", "y2", "y3", "y7")
  numeric <- corr_mat(simul_data, items_sel)
  type <- corr_mat(simul_data, items_sel, output = "type")
  both <- corr_mat(simul_data, items_sel, output = "both")

  expect_equal(both$numeric, numeric)
  expect_equal(both$type, type)

  corr.mat <- numeric
  diag(corr.mat) <- NA
  corr_val <- unique(sort(corr.mat))
  diff <- diff(corr_val)
  rhoH <- mdepriv(simul_data, items_sel, method = "bv", output = "rhoH")
  expect_equal(rhoH, mean(corr_val[which(diff == max(diff)) + c(0, 1)]))

  corr.mat <- corr_mat(simul_data, items_sel, corr_type = "pearson")
  diag(corr.mat) <- NA
  corr_val <- unique(sort(corr.mat))
  diff <- diff(corr_val)
  rhoH <- mdepriv(simul_data, items_sel, method = "bv", bv_corr_type = "pearson", output = "rhoH")
  expect_equal(rhoH, mean(corr_val[which(diff == max(diff)) + c(0, 1)]))
  })
