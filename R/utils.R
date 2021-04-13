corr_mat_ <- function(data, items, corr_type, sampling_weights) {
  weightedCorr_automated <- function(x, y, corr_type, sampling_weights) {
    n_distinct_x <- length(unique(x))
    n_distinct_y <- length(unique(y))

    if (n_distinct_x < n_distinct_y) {
      x_temp <- x
      y_temp <- y
      x <- y_temp
      y <- x_temp
      n_distinct_x <- length(unique(x))
      n_distinct_y <- length(unique(y))
    }

    if (corr_type == "mixed") {
      if (n_distinct_x <= 10 & n_distinct_y <= 10) {
        corr_type <- "polychoric"
      } else if (n_distinct_x > 10 & n_distinct_y <= 10) {
        corr_type <- "polyserial"
      } else {
        corr_type <- "pearson"
      }
    }

    if (all(x == y)) {
      1
    } else if (n_distinct_x == n_distinct_y) {
      corr_xy <- wCorr::weightedCorr(x = x, y = y, method = corr_type, weights = sampling_weights)
      corr_yx <- wCorr::weightedCorr(x = y, y = x, method = corr_type, weights = sampling_weights)
      mean(c(corr_xy, corr_yx))
    } else {
      wCorr::weightedCorr(x = x, y = y, method = corr_type, weights = sampling_weights)
    }
  }

  corr_mat <- matrix(NA, length(items), length(items), dimnames = list(items, items))
  for (i in items) {
    for (j in items) {
      corr_mat[i, j] <- weightedCorr_automated(x = data[, i], y = data[, j], corr_type = corr_type, sampling_weights = sampling_weights)
    }
  }

  corr_mat
}
# ------------------------------------------------------------------------
